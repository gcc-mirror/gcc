/* Subroutines needed for unwinding stack frames for exception handling.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@cygnus.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "defaults.h"

#ifdef DWARF2_UNWIND_INFO
#include "gansidecl.h"
#include "dwarf2.h"
#include <stddef.h>
#include "frame.h"

/* Don't use `fancy_abort' here even if config.h says to use it.  */
#ifdef abort
#undef abort
#endif

/* Some types used by the DWARF 2 spec.  */

typedef          int  sword __attribute__ ((mode (SI)));
typedef unsigned int  uword __attribute__ ((mode (SI)));
typedef unsigned int  uaddr __attribute__ ((mode (pointer)));
typedef          int  saddr __attribute__ ((mode (pointer)));
typedef unsigned char ubyte;

/* The first few fields of a CIE.  The CIE_id field is 0xffffffff for a CIE,
   to distinguish it from a valid FDE.  FDEs are aligned to an addressing
   unit boundary, but the fields within are unaligned.  */

struct dwarf_cie {
  uword length;
  sword CIE_id;
  ubyte version;
  char augmentation[0];
} __attribute__ ((packed, aligned (__alignof__ (void *))));

/* The first few fields of an FDE.  */

struct dwarf_fde {
  uword length;
  sword CIE_delta;
  void* pc_begin;
  uaddr pc_range;
} __attribute__ ((packed, aligned (__alignof__ (void *))));

typedef struct dwarf_fde fde;

/* Objects to be searched for frame unwind info.  */

static struct object *objects;

/* The information we care about from a CIE.  */

struct cie_info {
  char *augmentation;
  void *eh_ptr;
  int code_align;
  int data_align;
  unsigned ra_regno;
};

/* The current unwind state, plus a saved copy for DW_CFA_remember_state.  */

struct frame_state_internal
{
  struct frame_state s;
  struct frame_state_internal *saved_state;
};
  
/* Decode the unsigned LEB128 constant at BUF into the variable pointed to
   by R, and return the new value of BUF.  */

static void *
decode_uleb128 (unsigned char *buf, unsigned *r)
{
  unsigned shift = 0;
  unsigned result = 0;

  while (1)
    {
      unsigned byte = *buf++;
      result |= (byte & 0x7f) << shift;
      if ((byte & 0x80) == 0)
	break;
      shift += 7;
    }
  *r = result;
  return buf;
}

/* Decode the signed LEB128 constant at BUF into the variable pointed to
   by R, and return the new value of BUF.  */

static void *
decode_sleb128 (unsigned char *buf, int *r)
{
  unsigned shift = 0;
  unsigned result = 0;
  unsigned byte;

  while (1)
    {
      byte = *buf++;
      result |= (byte & 0x7f) << shift;
      shift += 7;
      if ((byte & 0x80) == 0)
	break;
    }
  if (shift < (sizeof (*r) * 8) && (byte & 0x40) != 0)
    result |= - (1 << shift);

  *r = result;
  return buf;
}

/* Read unaligned data from the instruction buffer.  */

union unaligned {
  void *p;
  unsigned b2 __attribute__ ((mode (HI)));
  unsigned b4 __attribute__ ((mode (SI)));
  unsigned b8 __attribute__ ((mode (DI)));
} __attribute__ ((packed));
static inline void *
read_pointer (void *p)
{ union unaligned *up = p; return up->p; }
static inline unsigned
read_1byte (void *p)
{ return *(unsigned char *)p; }
static inline unsigned
read_2byte (void *p)
{ union unaligned *up = p; return up->b2; }
static inline unsigned
read_4byte (void *p)
{ union unaligned *up = p; return up->b4; }
static inline unsigned long
read_8byte (void *p)
{ union unaligned *up = p; return up->b8; }

/* Ordering function for FDEs.  Functions can't overlap, so we just compare
   their starting addresses.  */

static inline saddr
fde_compare (fde *x, fde *y)
{
  return (saddr)x->pc_begin - (saddr)y->pc_begin;
}

/* Return the address of the FDE after P.  */

static inline fde *
next_fde (fde *p)
{
  return (fde *)(((char *)p) + p->length + sizeof (p->length));
}

/* One iteration of an insertion sort, for adding new FDEs to the array.
   Usually the new FDE will go in at the end, so we can expect close to
   O(n) performance.  If this turns out to be overly optimistic, we can have
   the linker sort the FDEs so we don't have to do it at run time.  */

static void
fde_insert (fde **array, size_t i, fde *this_fde)
{
  array[i] = this_fde;

  for (; i > 0 && fde_compare (array[i], array[i-1]) < 0; --i)
    {
      this_fde = array[i];
      array[i] = array[i-1];
      array[i-1] = this_fde;
    }
}

static size_t
count_fdes (fde *this_fde)
{
  size_t count;

  for (count = 0; this_fde->length != 0; this_fde = next_fde (this_fde))
    {
      /* Skip CIEs and linked once FDE entries.  */
      if (this_fde->CIE_delta == 0 || this_fde->pc_begin == 0)
	continue;

      ++count;
    }

  return count;
}

static void
add_fdes (fde *this_fde, fde **array, size_t *i_ptr,
	  void **beg_ptr, void **end_ptr)
{
  size_t i = *i_ptr;
  void *pc_begin = *beg_ptr;
  void *pc_end = *end_ptr;

  for (; this_fde->length != 0; this_fde = next_fde (this_fde))
    {
      /* Skip CIEs and linked once FDE entries.  */
      if (this_fde->CIE_delta == 0 || this_fde->pc_begin == 0)
	continue;

      fde_insert (array, i++, this_fde);

      if (this_fde->pc_begin < pc_begin)
	pc_begin = this_fde->pc_begin;
      if (this_fde->pc_begin + this_fde->pc_range > pc_end)
	pc_end = this_fde->pc_begin + this_fde->pc_range;
    }

  *i_ptr = i;
  *beg_ptr = pc_begin;
  *end_ptr = pc_end;
}

/* Set up a sorted array of pointers to FDEs for a loaded object.  We
   count up the entries before allocating the array because it's likely to
   be faster.  */

static void
frame_init (struct object* ob)
{
  fde *this_fde;
  size_t count;
  fde **array;
  void *pc_begin, *pc_end;

  if (ob->fde_array)
    {
      fde **p = ob->fde_array;
      for (count = 0; *p; ++p)
	count += count_fdes (*p);
    }
  else
    count = count_fdes (ob->fde_begin);

  ob->count = count;
  array = (fde **) malloc (sizeof (fde *) * count);

  pc_begin = (void*)(uaddr)-1;
  pc_end = 0;
  count = 0;

  if (ob->fde_array)
    {
      fde **p = ob->fde_array;
      for (; *p; ++p)
	add_fdes (*p, array, &count, &pc_begin, &pc_end);
    }
  else
    add_fdes (ob->fde_begin, array, &count, &pc_begin, &pc_end);

  ob->fde_array = array;
  ob->pc_begin = pc_begin;
  ob->pc_end = pc_end;
}

/* Return a pointer to the FDE for the function containing PC.  */

static fde *
find_fde (void *pc)
{
  struct object *ob;
  size_t lo, hi;

  for (ob = objects; ob; ob = ob->next)
    {
      if (ob->pc_begin == 0)
	frame_init (ob);
      if (pc >= ob->pc_begin && pc < ob->pc_end)
	break;
    }

  if (ob == 0)
    return 0;

  /* Standard binary search algorithm.  */
  for (lo = 0, hi = ob->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      fde *f = ob->fde_array[i];

      if (pc < f->pc_begin)
	hi = i;
      else if (pc > f->pc_begin + f->pc_range)
	lo = i + 1;
      else
	return f;
    }

  return 0;
}

static inline struct dwarf_cie *
get_cie (fde *f)
{
  return ((void *)&f->CIE_delta) - f->CIE_delta;
}

/* Extract any interesting information from the CIE for the translation
   unit F belongs to.  */

static void *
extract_cie_info (fde *f, struct cie_info *c)
{
  void *p;
  int i;

  c->augmentation = get_cie (f)->augmentation;

  if (strcmp (c->augmentation, "") != 0
      && strcmp (c->augmentation, "eh") != 0
      && c->augmentation[0] != 'z')
    return 0;

  p = c->augmentation + strlen (c->augmentation) + 1;

  if (strcmp (c->augmentation, "eh") == 0)
    {
      c->eh_ptr = read_pointer (p);
      p += sizeof (void *);
    }
  else
    c->eh_ptr = 0;

  p = decode_uleb128 (p, &c->code_align);
  p = decode_sleb128 (p, &c->data_align);
  c->ra_regno = *(unsigned char *)p++;

  /* If the augmentation starts with 'z', we now see the length of the
     augmentation fields.  */
  if (c->augmentation[0] == 'z')
    {
      p = decode_uleb128 (p, &i);
      p += i;
    }

  return p;
}

/* Decode one instruction's worth of of DWARF 2 call frame information.
   Used by __frame_state_for.  Takes pointers P to the instruction to
   decode, STATE to the current register unwind information, INFO to the
   current CIE information, and PC to the current PC value.  Returns a
   pointer to the next instruction.  */

static void *
execute_cfa_insn (void *p, struct frame_state_internal *state,
		  struct cie_info *info, void **pc)
{
  unsigned insn = *(unsigned char *)p++;
  unsigned reg;
  int offset;

  if (insn & DW_CFA_advance_loc)
    *pc += ((insn & 0x3f) * info->code_align);
  else if (insn & DW_CFA_offset)
    {
      reg = (insn & 0x3f);
      p = decode_uleb128 (p, &offset);
      offset *= info->data_align;
      state->s.saved[reg] = REG_SAVED_OFFSET;
      state->s.reg_or_offset[reg] = offset;
    }
  else if (insn & DW_CFA_restore)
    {
      reg = (insn & 0x3f);
      state->s.saved[reg] = REG_UNSAVED;
    }
  else switch (insn)
    {
    case DW_CFA_set_loc:
      *pc = read_pointer (p);
      p += sizeof (void *);
      break;
    case DW_CFA_advance_loc1:
      *pc += read_1byte (p);
      p += 1;
      break;
    case DW_CFA_advance_loc2:
      *pc += read_2byte (p);
      p += 2;
      break;
    case DW_CFA_advance_loc4:
      *pc += read_4byte (p);
      p += 4;
      break;

    case DW_CFA_offset_extended:
      p = decode_uleb128 (p, &reg);
      p = decode_uleb128 (p, &offset);
      offset *= info->data_align;
      state->s.saved[reg] = REG_SAVED_OFFSET;
      state->s.reg_or_offset[reg] = offset;
      break;
    case DW_CFA_restore_extended:
      p = decode_uleb128 (p, &reg);
      state->s.saved[reg] = REG_UNSAVED;
      break;

    case DW_CFA_undefined:
    case DW_CFA_same_value:
    case DW_CFA_nop:
      break;

    case DW_CFA_register:
      {
	unsigned reg2;
	p = decode_uleb128 (p, &reg);
	p = decode_uleb128 (p, &reg2);
	state->s.saved[reg] = REG_SAVED_REG;
	state->s.reg_or_offset[reg] = reg2;
      }
      break;

    case DW_CFA_def_cfa:
      p = decode_uleb128 (p, &reg);
      p = decode_uleb128 (p, &offset);
      state->s.cfa_reg = reg;
      state->s.cfa_offset = offset;
      break;
    case DW_CFA_def_cfa_register:
      p = decode_uleb128 (p, &reg);
      state->s.cfa_reg = reg;
      break;
    case DW_CFA_def_cfa_offset:
      p = decode_uleb128 (p, &offset);
      state->s.cfa_offset = offset;
      break;
      
    case DW_CFA_remember_state:
      {
	struct frame_state_internal *save =
	  (struct frame_state_internal *)
	  malloc (sizeof (struct frame_state_internal));
	memcpy (save, state, sizeof (struct frame_state_internal));
	state->saved_state = save;
      }
      break;
    case DW_CFA_restore_state:
      {
	struct frame_state_internal *save = state->saved_state;
	memcpy (state, save, sizeof (struct frame_state_internal));
	free (save);
      }
      break;

      /* FIXME: Hardcoded for SPARC register window configuration.  */
    case DW_CFA_GNU_window_save:
      for (reg = 16; reg < 32; ++reg)
	{
	  state->s.saved[reg] = REG_SAVED_OFFSET;
	  state->s.reg_or_offset[reg] = (reg - 16) * sizeof (void *);
	}
      break;

    case DW_CFA_GNU_args_size:
      p = decode_uleb128 (p, &offset);
      state->s.args_size = offset;
      break;

    default:
      abort ();
    }
  return p;
}

/* Called from crtbegin.o to register the unwind info for an object.  */

void
__register_frame_info (void *begin, struct object *ob)
{
  ob->fde_begin = begin;

  ob->pc_begin = ob->pc_end = 0;
  ob->fde_array = 0;
  ob->count = 0;

  ob->next = objects;
  objects = ob;
}

/* Similar, but BEGIN is actually a pointer to a table of unwind entries
   for different translation units.  Called from the file generated by
   collect2.  */

void
__register_frame_info_table (void *begin, struct object *ob)
{
  ob->fde_begin = begin;
  ob->fde_array = begin;

  ob->pc_begin = ob->pc_end = 0;
  ob->count = 0;

  ob->next = objects;
  objects = ob;
}

/* Called from crtend.o to deregister the unwind info for an object.  */

void
__deregister_frame_info (void *begin)
{
  struct object **p = &objects;

  while (*p)
    {
      if ((*p)->fde_begin == begin)
	{
	  struct object *ob = *p;
	  *p = (*p)->next;

	  /* If we've run init_frame for this object, free the FDE array.  */
	  if (ob->pc_begin)
	    free (ob->fde_array);

	  return;
	}
      p = &((*p)->next);
    }
  abort ();
}

/* Called from __throw to find the registers to restore for a given
   PC_TARGET.  The caller should allocate a local variable of `struct
   frame_state' (declared in frame.h) and pass its address to STATE_IN.  */

struct frame_state *
__frame_state_for (void *pc_target, struct frame_state *state_in)
{
  fde *f;
  void *insn, *end, *pc;
  struct cie_info info;
  struct frame_state_internal state;

  f = find_fde (pc_target);
  if (f == 0)
    return 0;

  insn = extract_cie_info (f, &info);
  if (insn == 0)
    return 0;

  memset (&state, 0, sizeof (state));
  state.s.retaddr_column = info.ra_regno;
  state.s.eh_ptr = info.eh_ptr;

  /* First decode all the insns in the CIE.  */
  end = next_fde ((fde*) get_cie (f));
  while (insn < end)
    insn = execute_cfa_insn (insn, &state, &info, 0);

  insn = ((fde *)f) + 1;

  if (info.augmentation[0] == 'z')
    {
      int i;
      insn = decode_uleb128 (insn, &i);
      insn += i;
    }

  /* Then the insns in the FDE up to our target PC.  */
  end = next_fde (f);
  pc = f->pc_begin;
  while (insn < end && pc <= pc_target)
    insn = execute_cfa_insn (insn, &state, &info, &pc);

  memcpy (state_in, &state.s, sizeof (state.s));
  return state_in;
}
#endif /* DWARF2_UNWIND_INFO */
