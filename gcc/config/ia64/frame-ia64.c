/* Subroutines needed for unwinding IA-64 standard format stack frame
   info for exception handling.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod  <amacleod@cygnus.com>
                  Andrew Haley  <aph@cygnus.com>

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

/* We disable this when inhibit_libc, so that gcc can still be built without
   needing header files first.  */
/* ??? This is not a good solution, since prototypes may be required in
   some cases for correct code.  See also libgcc2.c/crtstuff.c.  */
#ifndef inhibit_libc
#include <stdlib.h>
#include <unistd.h>
#else
#include <stddef.h>
#endif

#include "defaults.h"
#include "frame-ia64.h"
#include "eh-common.h"

/* Some types used by the DWARF 2 spec.  */

typedef          int  sword __attribute__ ((mode (SI)));
typedef unsigned int  uword __attribute__ ((mode (SI)));
typedef unsigned int  uaddr __attribute__ ((mode (pointer)));
typedef          int  saddr __attribute__ ((mode (pointer)));
typedef unsigned char ubyte;

#include "frame.h" 

/* Decode the unsigned LEB128 constant at BUF and return it. The value at
   MEM is updated to reflect the next position in the buffer.  */

static unsigned long
read_uleb128 (unsigned char **mem)
{
  unsigned shift = 0;
  unsigned long result = 0;
  unsigned char *buf = *mem;

  while (1)
    {
      unsigned long byte = *buf++;
      result |= (byte & 0x7f) << shift;
      if ((byte & 0x80) == 0)
        break;
      shift += 7;
    }
  *mem = buf;
  return result;
}


static unsigned char *
read_R_record (unwind_record *data, unsigned char val, unsigned char *ptr)
{
  if ((val & 0x40) == 0)
    {
      /* R1 format.  */
      if (val & 0x20)
        data->type = body;
      else
        data->type = prologue;
      data->record.r.rlen = (val & 0x1f);
      return ptr;
    }

  if ((val & 0xF8) == UNW_R2)
    {
      /* R2 format.  */
      unsigned char mask = (val & 0x07) << 1;
      if (*ptr & 0x80) 
        mask = mask | 1;
      data->type = prologue_gr;
      data->record.r.mask = mask;
      data->record.r.grsave = (*ptr++ & 0x7f);
      data->record.r.rlen = read_uleb128 (&ptr);
      return ptr;
    }

  if ((val & 0xFC) == UNW_R3)
    {
      /* R3 format.  */
      val = (val & 0x03);
      if (val == 0)
        data->type = prologue;
      else
        if (val == 1)
	  data->type = body;
	else
	  abort ();
      data->record.r.rlen = read_uleb128 (&ptr);
      return ptr;
    }
  abort ();
}

static void
process_a_b_reg_code(unwind_record *data, unsigned char val)
{
  int code = (val & 0x60) >> 5;
  int reg = (val & 0x1f);
  switch (code) 
    {
      case 0:
	data->record.x.reg = GR_REG (reg);
        break;
      case 1:
	data->record.x.reg = FR_REG (reg);
        break;
      case 2:
	data->record.x.reg = BR_REG (reg);
        break;
      case 3:
        /* TODO. We need to encode the specialty regs here. The table is 
	   on page B-9 of the runtime manual (under the X1 description.) */
        break;
    }
}

static unsigned char *
read_X_record (unwind_record *data, unsigned char val, unsigned char *ptr)
{
  unsigned long tmp;
  int byte1, byte2;
  switch (val) 
    {
      case UNW_X1:
        byte1 = *ptr++;
	data->record.x.t = read_uleb128 (&ptr);
	tmp = read_uleb128 (&ptr);
	if ((byte1 & 0x80) == 0)
	  {
	    data->type = spill_psprel;
	    data->record.x.pspoff = tmp;
	  }
	else
	  {
	    data->type = spill_sprel;
	    data->record.x.spoff = tmp;
	  }
	process_a_b_reg_code (data, byte1);
	return ptr;
      case UNW_X4:
        byte1 = *ptr++;
	data->record.x.qp = PR_REG (byte1 & 0x3f);
	data->type = spill_reg_p;
      case UNW_X2:
        {
	  int xy;
	  int treg;
	  /* Only set type if we didn't fall through the UNW_X4 case.  */
	  if (val == UNW_X2)
	    data->type = spill_reg;
	  byte1 = *ptr++;
	  byte2 = *ptr++;
	  process_a_b_reg_code (data, byte1);
	  xy = (((byte1 >> 7) << 1 ) | (byte2 >> 7));
	  treg = (byte2 & 0x7f);
	  switch (xy) 
	    {
	      case 0:
	        data->record.x.treg = GR_REG (treg);
	        break;
	      case 1:
	        data->record.x.treg = FR_REG (treg);
	        break;
	      case 2:
	        data->record.x.treg = BR_REG (treg);
	        break;
	      case 3:
	        abort ();
	    }
	  data->record.x.t = read_uleb128 (&ptr);
        }
	return ptr;
      case UNW_X3:
        byte1 = *ptr++;
	byte2 = *ptr++;
	data->record.x.qp = PR_REG (byte1 & 0x3f);
	process_a_b_reg_code (data, byte2);
	data->record.x.t = read_uleb128 (&ptr);
	tmp = read_uleb128 (&ptr);
	if ((byte1 & 0x80) == 0)
	  {
	    data->type = spill_psprel_p;
	    data->record.x.pspoff = tmp;
	  }
	else
	  {
	    data->type = spill_sprel_p;
	    data->record.x.spoff = tmp;
	  }	
	return ptr;
      default:
	abort ();
    }
  return NULL;
}

static unsigned char *
read_B_record (unwind_record *data, unsigned char val, unsigned char *ptr)
{
  if ((val & 0xc0) == 0x80)
    {
      /* B1 format.  */
      if ((val & 0x20) == 0)
        data->type = label_state;
      else
        data->type = copy_state;
      data->record.b.label = (val & 0x1f);
      return ptr;
    }
  
  if ((val & 0xe0) == 0xc0)
    {
      /* B2 format.  */
      data->type = epilogue;
      data->record.b.ecount = (val & 0x1f);
      data->record.b.t = read_uleb128 (&ptr);
      return ptr;
    }

  if (val == UNW_B3)
    {
      /* B3 format.  */
      data->type = epilogue;
      data->record.b.t = read_uleb128 (&ptr);
      data->record.b.ecount = read_uleb128 (&ptr);
      return ptr;
    }

  if (val == UNW_B4)
    {
      /* B4 format, with r == 0.  */
      data->type = label_state;
      data->record.b.label = read_uleb128 (&ptr);
      return ptr;
    }

  if (val == (UNW_B4 | 0x08))
    {
      /* B4 format, with r == 1.  */
      data->type = copy_state;
      data->record.b.label = read_uleb128 (&ptr);
      return ptr;
    }
  abort ();
}

/* This array is used to set the TYPE field for format P3.  */
static unw_record_type const P3_record_types[] = {
  psp_gr, rp_gr, pfs_gr, preds_gr, unat_gr, lc_gr, rp_br, rnat_gr,
  bsp_gr, bspstore_gr, fpsr_gr, priunat_gr
};

/* This array is used to set the TYPE field for format P7.  */
static unw_record_type const P7_record_types[] = {
  mem_stack_f, mem_stack_v, spill_base, psp_sprel, rp_when, rp_psprel,
  pfs_when, pfs_psprel, preds_when, preds_psprel, lc_when, lc_psprel,
  unat_when, unat_psprel, fpsr_when, fpsr_psprel
};

/* These values and the array are used to determine which additional ULEB128
   fields are required for the P7 format.  */
#define P7_T_SIZE	0
#define P7_T		1
#define P7_PSPOFF       2
#define P7_SPOFF	3
static unsigned char const P7_additional_fields [] = {
   P7_T_SIZE, P7_T, P7_PSPOFF, P7_SPOFF, P7_T, P7_PSPOFF, 
   P7_T, P7_PSPOFF, P7_T, P7_PSPOFF, P7_T, P7_PSPOFF, P7_T, P7_PSPOFF
};

/* This array is used to set the TYPE field for format P8. 
   Note that entry 0 is not used in this array, so it is filled with
   rp_spel for completely arbitrary reasons.  */
static unw_record_type const P8_record_types[] = {
  rp_sprel, rp_sprel, pfs_sprel, preds_sprel, lc_sprel, unat_sprel, fpsr_sprel, 
  bsp_when, bsp_psprel, bsp_sprel, bspstore_when, bspstore_psprel,
  bspstore_sprel, rnat_when, rnat_psprel, rnat_sprel, priunat_when_gr,
  priunat_psprel, priunat_sprel, priunat_when_mem
};

/* These values and the array are used to determine which additional ULEB128
   fields are required for the P8 format.  */
#define P8_T		0
#define P8_PSPOFF       1
#define P8_SPOFF	2
static unsigned char const P8_additional_fields [] = {
  P8_SPOFF, P8_SPOFF, P8_SPOFF, P8_SPOFF, P8_SPOFF, P8_SPOFF,
  P8_T, P8_PSPOFF, P8_SPOFF, P8_T, P8_PSPOFF, P8_SPOFF,
  P8_T, P8_PSPOFF, P8_SPOFF, P8_T, P8_PSPOFF, P8_SPOFF, P8_T
};


static unsigned char *
read_P_record (unwind_record *data, unsigned char val, unsigned char *ptr,
	       unwind_record *header)
{
  if ((val & 0xe0) == 0x80)
    {
      /* P1 format.  */
      data->type = br_mem;
      data->record.p.brmask = (val & 0x1f);
      return ptr;
    }

  if ((val & 0xf0) == 0xa0)
    {
      /* P2 format.  */
      int byte1;
      data->type = br_gr;
      byte1 = *ptr++;
      data->record.p.brmask = ((val & 0x0f) << 1) + (byte1 >> 7);
      data->record.p.gr = GR_REG (byte1 & 0x7f);
      return ptr;
    }

  if ((val & 0xf8) == 0xB0)
    {
      /* P3 format.  */
      int byte1 = *ptr++;
      int r = ((val & 0x07) << 1) + (byte1 >> 7);
      data->type = P3_record_types[r];
      if (r == 6)
        data->record.p.br = BR_REG (byte1 & 0x7f);
      else
        data->record.p.gr = GR_REG (byte1 & 0x7f);
      if (r > 11)
        abort ();
      return ptr;
    }

  if (val == UNW_P4)
    {
      /* P4 format.  */
      int size = (header->record.r.rlen * 2 + 7) / 8;

      data->type = spill_mask;
      data->record.p.imask = ptr;
      return ptr+size;
    }

  if (val == UNW_P5)
    {
      /* P5 format.  */
      int byte1 = *ptr++;
      int byte2 = *ptr++;
      int byte3 = *ptr++;
      data->type = frgr_mem;
      data->record.p.grmask = (byte1 >> 4);
      data->record.p.frmask = ((byte1 & 0x0f) << 16) | (byte2 << 8) | byte3;
      return ptr;
    }
  
  if ((val & 0xe0) == UNW_P6)
    {
      /* P6 format.  */
      if ((val & 0x10) == 0)
        data->type = fr_mem;
      else
        data->type = gr_mem;
      data->record.p.rmask = (val & 0x0f);
      return ptr;
    }
  
  if ((val & 0xf0) == UNW_P7)
    {
      /* P7 format.  */
      int r = (val & 0x0f);
      data->type = P7_record_types[r];
      switch (P7_additional_fields[r])
        {
	  case P7_T_SIZE:
	    data->record.p.t = read_uleb128 (&ptr);
	    data->record.p.size = read_uleb128 (&ptr) << 4;
	    break;
	  case P7_T:
	    data->record.p.t = read_uleb128 (&ptr);
	    break;
	  case P7_PSPOFF:
	    data->record.p.pspoff = read_uleb128 (&ptr);
	    break;
	  case P7_SPOFF:
	    data->record.p.spoff = read_uleb128 (&ptr);
	    break;
	}
      return ptr;
    }
 
  if (val == UNW_P8)
    {
      /* P8 format.  */
      int r = *ptr++;
      data->type = P8_record_types[r];
      switch (P8_additional_fields[r])
        {
	  case P8_T:
	    data->record.p.t = read_uleb128 (&ptr);
	    break;
	  case P8_PSPOFF:
	    data->record.p.pspoff = read_uleb128 (&ptr);
	    break;
	  case P8_SPOFF:
	    data->record.p.spoff = read_uleb128 (&ptr);
	    break;
	}
      return ptr;
    }
  
  if (val == UNW_P9)
    {
      /* P9 format.  */
      int byte1 = *ptr++;
      int byte2 = *ptr++;
      data->type = gr_gr;
      data->record.p.grmask = (byte1 & 0x0f);
      data->record.p.gr = GR_REG (byte2 & 0x7f);
      return ptr;
    }
  
  if (val == UNW_P10)
    {
#if 0
      /* P10 format.  */
      int abi = ptr[0];
      int context = ptr[1];
      /* TODO. something about abi entries.  */
#endif
      return ptr + 2;
    }

  return ptr;
}

/* This routine will determine what type of record the memory pointer
   is refering to, and fill in the appropriate fields for that record type. 
   HEADER is a pointer to the last region header unwind record.
   DATA is a pointer to an unwind record which will be filled in.
   PTR is a pointer to the current location in the unwind table where we
   will read the next record from.  
   The return value is the start of the next record.  */

static unsigned char *
get_unwind_record (unwind_record *header, unwind_record *data,
		   unsigned char *ptr)
{
  unsigned char val = *ptr++;

  if ((val & 0x80) == 0)
    return read_R_record (data, val, ptr);

  if (val == UNW_X1 || val == UNW_X2 || val == UNW_X3 || val == UNW_X4)
    return read_X_record (data, val, ptr);

  if (header->type != body)
    return read_P_record (data, val, ptr, header);
  else
    return read_B_record (data, val, ptr);
}

/* Frame processing routines.  */

/* Initialize a single register structure.  */
static inline void 
init_ia64_reg_loc (ia64_reg_loc *reg, short size)
{
  reg->when = -1;
  reg->loc_type = IA64_UNW_LOC_TYPE_NONE;
  reg->l.mem = (void *)0;
  reg->reg_size = size;
}

/* Iniitialize an entire frame to the default of nothing.  */
static void
init_ia64_unwind_frame (ia64_frame_state *frame) 
{
  int x;
  
  for (x = 0; x < 4; x++)
    init_ia64_reg_loc (&frame->gr[x], 8);
  for (x = 0; x < 20; x++)
    init_ia64_reg_loc (&frame->fr[x], 16);
  for (x = 0; x < 5; x++)
    init_ia64_reg_loc (&frame->br[x], 8);

  init_ia64_reg_loc (&frame->rp, 8);
  init_ia64_reg_loc (&frame->fpsr, 8);
  init_ia64_reg_loc (&frame->bsp, 8);
  init_ia64_reg_loc (&frame->bspstore, 8);
  init_ia64_reg_loc (&frame->rnat, 8);
  init_ia64_reg_loc (&frame->pfs, 8);
  init_ia64_reg_loc (&frame->unat, 8);
  init_ia64_reg_loc (&frame->lc, 8);
  init_ia64_reg_loc (&frame->pr, 8);
  init_ia64_reg_loc (&frame->priunat, 8);
  init_ia64_reg_loc (&frame->sp, 8);
  init_ia64_reg_loc (&frame->psp, 8);
  init_ia64_reg_loc (&frame->spill_base, 8);
}

/* This fuction will process a single descriptor.
   addr is a pointer to the descriptor record to read, 
   frame is the current frame state structure, which will be
     modified to reflect this descriptor.
   len is the length of a prologue region, or -1 if it wasn't one.
   the return value is a pointer to the start of the next descriptor.  */

static void *
execute_one_ia64_descriptor (void *addr, ia64_frame_state *frame, long *len)
{
  /* The last region_header.  Needed to distinguish between prologue and body
     descriptors.  Also needed for length of P4 format.  */
  static unwind_record region_header;

  unwind_record r;
  ia64_reg_loc *loc_ptr = NULL;
  int grmask = 0, frmask = 0;

  *len = -1;
  addr = get_unwind_record (&region_header, &r, addr);

  /* Process it in 2 phases, the first phase will either do the work,
     or set up a pointer to the records we care about 
     (ie a special purpose ar perhaps, and the second will actually 
     fill in the record.  */
  switch (r.type) 
    {
      case prologue:
      case body:
	*len = r.record.r.rlen;
	region_header = r;
	break;
      case prologue_gr:
        {
	  int val, reg;

	  *len = r.record.r.rlen;
	  val = r.record.r.mask;
	  reg = r.record.r.grsave;
	  if (val & 0x08)
	    {
	      frame->rp.when = 0;
	      frame->rp.loc_type  = IA64_UNW_LOC_TYPE_GR;
	      frame->rp.l.regno = reg++;
	    }
	  if (val & 0x04)
	    {
	      frame->pfs.when = 0;
	      frame->pfs.loc_type  = IA64_UNW_LOC_TYPE_GR;
	      frame->pfs.l.regno = reg++;
	    }
	  if (val & 0x02)
	    {
	      frame->psp.when = 0;
	      frame->psp.loc_type  = IA64_UNW_LOC_TYPE_GR;
	      frame->psp.l.regno = reg++;
	    }
	  if (val & 0x01)
	    {
	      frame->pr.when = 0;
	      frame->pr.loc_type  = IA64_UNW_LOC_TYPE_GR;
	      frame->pr.l.regno = reg++;
	    }
	  region_header = r;
	  break;
	}
      case mem_stack_f:
	frame->sp.l.offset = r.record.p.size;
	frame->sp.loc_type = IA64_UNW_LOC_TYPE_OFFSET;
        frame->sp.when = r.record.p.t; 
	break;
      case mem_stack_v:
	frame->psp.when = r.record.p.t;
	break;
      case psp_gr:
      case psp_sprel:
        loc_ptr = &frame->psp;
	break;
      case rp_br:
      case rp_gr:
      case rp_when:
      case rp_psprel:
      case rp_sprel:
        loc_ptr = &frame->rp;
	break;
      case pfs_gr:
      case pfs_when:
      case pfs_psprel:
      case pfs_sprel:
        loc_ptr = &frame->pfs;
	break;
      case preds_gr:
      case preds_when:
      case preds_psprel:
      case preds_sprel:
        loc_ptr = &frame->pr;
	break;
      case unat_gr:
      case unat_when:
      case unat_psprel:
      case unat_sprel:
        loc_ptr = &frame->unat;
	break;
      case lc_gr:
      case lc_when:
      case lc_psprel:
      case lc_sprel:
        loc_ptr = &frame->lc;
	break;
      case fpsr_gr:
      case fpsr_when:
      case fpsr_psprel:
      case fpsr_sprel:
        loc_ptr = &frame->fpsr;
	break;
      case priunat_gr:
      case priunat_sprel:
      case priunat_when_gr:
      case priunat_when_mem:
      case priunat_psprel:
        loc_ptr = &frame->priunat;
	break;
      case bsp_gr:
      case bsp_sprel:
      case bsp_when:
      case bsp_psprel:
        loc_ptr = &frame->bsp;
	break;
      case bspstore_gr:
      case bspstore_sprel:
      case bspstore_when:
      case bspstore_psprel:
        loc_ptr = &frame->bspstore;
	break;
      case rnat_gr:
      case rnat_sprel:
      case rnat_when:
      case rnat_psprel:
        loc_ptr = &frame->rnat;
	break;
      case spill_base:
        loc_ptr = &frame->spill_base;
	break;
      case fr_mem:
        frmask = r.record.p.rmask;
	break;
      case gr_mem:
        grmask = r.record.p.rmask;
	break;
      case frgr_mem:
        frmask = r.record.p.frmask;
        grmask = r.record.p.grmask;
	break;
      case br_mem:
        {
	  int x, mask = 0x01;
	  int saved = r.record.p.brmask;
	  for (x = 0; x < 5; x++)
	    {
	      if (saved & mask)
		frame->br[x].loc_type = IA64_UNW_LOC_TYPE_SPILLBASE;
	      mask = mask << 1;
	    }
	  break;
	}
      case br_gr:
        {
	  int x, mask = 0x01;
	  int reg = r.record.p.gr;
	  int saved = r.record.p.brmask;
	  for (x = 0; x < 5; x++)
	    {
	      if (saved & mask)
	        {
		  frame->br[x].loc_type = IA64_UNW_LOC_TYPE_GR;
		  frame->br[x].l.regno = reg++;
		}
	      mask = mask << 1;
	    }
	  break;
	}
      case gr_gr:
        {
	  int x, mask = 0x01;
	  int reg = r.record.p.gr;
	  int saved = r.record.p.grmask;
	  for (x = 0; x < 4; x++)
	    {
	      if (saved & mask)
	        {
		  frame->br[x].loc_type = IA64_UNW_LOC_TYPE_GR;
		  frame->br[x].l.regno = reg++;
		}
	      mask = mask << 1;
	    }
	  break;
	}
      case spill_mask:
        /* TODO.  */
	break;
      case epilogue:
        /* TODO.  */
	break;
      case label_state:
        /* TODO.  */
	break;
      case copy_state: 
        /* TODO. */
	break;
      case spill_psprel:
      case spill_sprel:
      case spill_reg:
      case spill_psprel_p:
      case spill_sprel_p:
      case spill_reg_p:
        /* TODO. */
	break;
      default:
	abort ();
	break;
    }

  if (frmask)
    {
      int x, mask = 0x01;
      for (x = 0; x < 20; x++)
	{
	  if (frmask & mask)
	    frame->fr[x].loc_type = IA64_UNW_LOC_TYPE_SPILLBASE;
	  mask = mask << 1;
	}
    }

  if (grmask)
    {
      int x, mask = 0x01;
      for (x = 0; x < 4; x++)
	{
	  if (grmask & mask)
	    frame->gr[x].loc_type = IA64_UNW_LOC_TYPE_SPILLBASE;
	  mask = mask << 1;
	}
    }

  /* If there is more to do:  */
  if (loc_ptr != NULL)
    switch (r.type) 
      {
	case psp_gr:
	case rp_gr:
	case pfs_gr:
	case preds_gr:
	case unat_gr:
	case lc_gr:
	case fpsr_gr:
	case priunat_gr:
	case bsp_gr:
	case bspstore_gr:
	case rnat_gr:
	  loc_ptr->loc_type = IA64_UNW_LOC_TYPE_GR;
	  loc_ptr->l.regno = r.record.p.gr;
	  break;
	case rp_br:
	  loc_ptr->loc_type = IA64_UNW_LOC_TYPE_BR;
	  loc_ptr->l.regno = r.record.p.br;
	  break;
	case rp_when:
	case pfs_when:
	case preds_when:
	case unat_when:
	case lc_when:
	case fpsr_when:
	case priunat_when_gr:
	case priunat_when_mem:
	case bsp_when:
	case bspstore_when:
	case rnat_when:
	  loc_ptr->when = r.record.p.t;
	  break;
	case rp_psprel:
	case pfs_psprel:
	case preds_psprel:
	case unat_psprel:
	case lc_psprel:
	case fpsr_psprel:
	case priunat_psprel:
	case bsp_psprel:
	case bspstore_psprel:
	case rnat_psprel:
	case spill_base:
	  loc_ptr->loc_type = IA64_UNW_LOC_TYPE_PSPOFF;
	  loc_ptr->l.offset = r.record.p.pspoff;
	  break;
	case psp_sprel:
	case rp_sprel:
	case pfs_sprel:
	case preds_sprel:
	case unat_sprel:
	case lc_sprel:
	case fpsr_sprel:
	case priunat_sprel:
	case bsp_sprel:
	case bspstore_sprel:
	case rnat_sprel:
	  loc_ptr->loc_type = IA64_UNW_LOC_TYPE_SPOFF;
	  loc_ptr->l.offset = r.record.p.spoff;
	  break;
	default:
	  abort ();
	  break;
      }
  return addr;
}


#define IS_NaT_COLLECTION_ADDR(addr) ((((long)(addr) >> 3) & 0x3f) == 0x3f)

/* Returns the address of the slot that's NSLOTS slots away from
   the address ADDR. NSLOTS may be positive or negative. */
static void *
rse_address_add(unsigned char *addr, int nslots)
{
  unsigned char *new_addr;
  int mandatory_nat_slots = nslots / 63;
  int direction = nslots < 0 ? -1 : 1;

  new_addr = addr + 8 * (nslots + mandatory_nat_slots);

  if (((long)new_addr >> 9)
      != ((long)(addr + 8 * 64 * mandatory_nat_slots) >> 9))
    new_addr += 8 * direction;

  if (IS_NaT_COLLECTION_ADDR(new_addr))
    new_addr += 8 * direction;

  return new_addr;
}


/* Normalize a record to originate in either a register or memory 
   location.  */
static void
normalize_reg_loc (ia64_frame_state *frame, ia64_reg_loc *reg)
{
  unsigned char *tmp;
  switch (reg->loc_type)
    {
      case IA64_UNW_LOC_TYPE_MEM:
        /* Already done.  */
        break;
      case IA64_UNW_LOC_TYPE_GR:
        /* If the register its saved in is a LOCAL register, we know
	   its actually in memory, so we'll pick it up from there.  */
        if (reg->l.regno >= 32 && frame->my_bsp != 0)
	  {
	   /* Get from backing store.  */
	    tmp = rse_address_add(frame->my_bsp, reg->l.regno - 32);
	    reg->l.mem = tmp;
	    reg->loc_type = IA64_UNW_LOC_TYPE_MEM;
	  }
        break;
      case IA64_UNW_LOC_TYPE_FR:
        /* If the register its saved in is a LOCAL register, we know
	   its actually in memory, so we'll pick it up from there.  */
        if (reg->l.regno >= 32)
	  {
	   /* TODO. get from backing store.  */
	  }
        break;
      case IA64_UNW_LOC_TYPE_BR:
        break;
      case IA64_UNW_LOC_TYPE_SPOFF:
        /* Offset from the stack pointer, calculate the memory address
	   now.  */
	tmp = (unsigned char *)frame->my_sp + reg->l.offset * 4;
	reg->l.mem = tmp;
	reg->loc_type = IA64_UNW_LOC_TYPE_MEM;
        break;
      case IA64_UNW_LOC_TYPE_PSPOFF:
        /* Actualy go get the value of the PSP add the offset, and thats 
	   the mem location we can find this value at. */
	tmp = (unsigned char *)frame->my_psp + 16 - reg->l.offset * 4;
	reg->l.mem = tmp;
	reg->loc_type = IA64_UNW_LOC_TYPE_MEM;
        break;
      case IA64_UNW_LOC_TYPE_SPILLBASE:
        /* located at the current spill base memory location, and we
	   have to bump it as well. */
	reg->l.mem = frame->spill_base.l.mem;
	reg->loc_type = IA64_UNW_LOC_TYPE_MEM;
	frame->spill_base.l.mem += 8;
        break;
    }

}

/* This function looks at a reg_loc and determines if its going
   to be an executed record or not between time start and end.  
   It is executed if it is exectued at START time. It is NOT
   executed if it happens at END time. */
static void 
maybe_normalize_reg_loc (ia64_frame_state *frame, ia64_reg_loc *reg,
			 long start, long end)
{
  if (reg->loc_type != IA64_UNW_LOC_TYPE_NONE 
      && reg->when >= start && reg->when < end)
    normalize_reg_loc (frame, reg);
}


/* Only works for 8 byte or less registers.  */
void *
__get_real_reg_value (ia64_reg_loc *reg)
{
  if (reg->loc_type == IA64_UNW_LOC_TYPE_MEM)
    return *((void **)(reg->l.mem));
  
  /* All registers should be in memory if we've saved them. Local 
     registers will be in backing store.  */
  abort ();
}

void
__set_real_reg_value (ia64_reg_loc *reg, void *val) 
{
  if (reg->loc_type == IA64_UNW_LOC_TYPE_MEM)
    {
      void **ptr = reg->l.mem;
      *ptr = val;
      return;
    }
  abort ();
}

static void
copy_reg_value (ia64_reg_loc *src, ia64_reg_loc *dest)
{
  void **p = dest->l.mem;
  if (src->loc_type == IA64_UNW_LOC_TYPE_NONE)
    return;
  
  if (src->reg_size != dest->reg_size)
    abort ();
  if (src->reg_size <= 8)
    *p = __get_real_reg_value (src);
  else
    {
      void **d;
      if (src->reg_size > 16)
        abort ();
      if (dest->loc_type != IA64_UNW_LOC_TYPE_MEM)
        abort ();
      d = (void **)(dest->l.mem);
      *p++ = *d++;
      *p = *d;
    }
  return;
}

/* Copy the values of any relevant saved registers in one frame 
   to another for unwinding.  */
void 
__copy_saved_reg_state (ia64_frame_state *dest, ia64_frame_state *src)
{
  int x;
  for (x = 0; x < 4 ; x++)
    copy_reg_value (&src->gr[x], &dest->gr[x]);
  for (x = 0; x < 20 ; x++)
    copy_reg_value (&src->fr[x], &dest->fr[x]);
  for (x = 0; x < 5 ; x++)
    copy_reg_value (&src->br[x], &dest->br[x]);
      
  copy_reg_value (&src->fpsr, &dest->fpsr);
  copy_reg_value (&src->rnat, &dest->rnat);
  copy_reg_value (&src->unat, &dest->unat);
  copy_reg_value (&src->lc, &dest->lc);
  copy_reg_value (&src->pr, &dest->pr);
  copy_reg_value (&src->priunat, &dest->priunat);
  copy_reg_value (&src->pfs, &dest->pfs);
}


static void 
process_state_between (ia64_frame_state *frame, long start, long end)
{
  int x;
  /* PSP, RP, SP, and PFS are handled seperately from here. */

  /* GR's, FR's and BR's are saved at an arbitrary point, so we
     should handle them at the very beginning.  */
  /* ??? Err, no they aren't.  There's the spill_mask record that
     tells us when each is processed.  */
  if (start == 0)
    {
      for (x = 0; x < 4 ; x++)
	normalize_reg_loc (frame, &frame->gr[x]);
      for (x = 0; x < 20 ; x++)
	normalize_reg_loc (frame, &frame->fr[x]);
      for (x = 0; x < 5 ; x++)
	normalize_reg_loc (frame, &frame->br[x]);
    }
  
  maybe_normalize_reg_loc (frame, &frame->fpsr, start, end);
  maybe_normalize_reg_loc (frame, &frame->bsp, start, end);
  maybe_normalize_reg_loc (frame, &frame->bspstore, start, end);
  maybe_normalize_reg_loc (frame, &frame->rnat, start, end);
  maybe_normalize_reg_loc (frame, &frame->unat, start, end);
  maybe_normalize_reg_loc (frame, &frame->lc, start, end);
  maybe_normalize_reg_loc (frame, &frame->pr, start, end);
  maybe_normalize_reg_loc (frame, &frame->priunat, start, end);
}

/* This function will take a frame state, and translate all the location
   records into actual memory address, or register numbers, based on
   what the ia64_reg_loc fields require to actually go get the values.  
   (ie, this translates SPOFF and PSPOFF, etc into MEM types. 
   frame is the frame to be changed.
   unwind_time is the insn slot number we are unwinding to.  Anything 
     that has a WHEN record beyond this time is cleared since it
     isn't relevant.  */
static void
frame_translate (ia64_frame_state *frame, long unwind_time)
{
  /* ??? Is this supposed to mark the end of the stack?  */
  if (frame->rp.loc_type == IA64_UNW_LOC_TYPE_NONE)
    return;

  /* At function entry, SP == PSP.  */
  frame->my_psp = frame->my_sp;
  if (frame->psp.loc_type != IA64_UNW_LOC_TYPE_NONE)
    {
      /* We've saved a frame pointer somewhere.  This will be the
	 canonical PSP for the function.  */
      normalize_reg_loc (frame, &frame->psp);
      if (frame->psp.when < unwind_time)
	frame->my_psp = __get_real_reg_value (&frame->psp);
    }
  else if (frame->sp.loc_type == IA64_UNW_LOC_TYPE_OFFSET)
    {
      /* We've a fixed sized stack frame.  The PSP is at a known offset.  */
	
      if (frame->sp.when < unwind_time)
        frame->my_psp = frame->my_sp + frame->sp.l.offset;
    }
  /* Otherwise the stack frame size was zero and no adjustment needed.  */

  /* Find PFS, RP and the spill base.  All of which might have
     addresses based off the PSP computed above.  */
  normalize_reg_loc (frame, &frame->pfs);
  normalize_reg_loc (frame, &frame->rp);

  if (frame->spill_base.loc_type != IA64_UNW_LOC_TYPE_NONE)
    normalize_reg_loc (frame, &frame->spill_base);
  else
    {
      /* Otherwise we're supposed to infer it from the size of the
	 saved GR/BR/FR registers, putting the top at psp+16.  */
      long size = 0, i;
      for (i = 0; i < 4; ++i)
	if (frame->gr[i].when >= 0)
	  size += 8;
      for (i = 0; i < 5; ++i)
	if (frame->br[i].when >= 0)
	  size += 8;
      for (i = 0; i < 20; ++i)
	if (frame->fr[i].when >= 0)
	  size += 16;
      frame->spill_base.l.mem = frame->my_psp + 16 - size;
    }

  /* If the SP is adjusted, process records up to where it
     is adjusted, then adjust it, then process the rest.  */
  if (frame->sp.when >= 0)
    {
      process_state_between (frame, 0, frame->sp.when);
      if (frame->sp.loc_type != IA64_UNW_LOC_TYPE_OFFSET)
	abort ();
      frame->my_sp = frame->my_psp - frame->sp.l.offset;
      process_state_between (frame, frame->sp.when, unwind_time);
    }
  else
    process_state_between (frame, 0, unwind_time);
}

/* This function will set a frame_state with all the required fields
   from a functions unwind descriptors.
   pc is the location we need info up until (ie, the unwind point)
   frame is the frame_state structure to be set up.
   Returns a pointer to the unwind info pointer for the frame.  */
unwind_info_ptr *
__build_ia64_frame_state (unsigned char *pc, ia64_frame_state *frame,
			  void *bsp, void *sp, void **pc_base_ptr)
{
  long len;
  int region_offset = 0;
  int last_region_size = 0;
  void *addr, *end;
  unwind_table_entry *entry;
  unsigned char *start_pc;
  void *pc_base;
  int pc_offset;
  struct unwind_info_ptr *unw_info_ptr;

  entry = __ia64_find_fde (pc, &pc_base);
  if (!entry)
    return 0;

  start_pc = pc_base + entry->start_offset;
  unw_info_ptr = ((struct unwind_info_ptr *)(pc_base + entry->unwind_offset));
  addr = unw_info_ptr->unwind_descriptors;
  end = addr + IA64_UNW_HDR_LENGTH (unw_info_ptr->header) * 8;
  pc_offset = (pc - start_pc) / 16 * 3;

  init_ia64_unwind_frame (frame);
  frame->my_bsp = bsp;
  frame->my_sp = sp;

  /* Stop when we get to the end of the descriptor list, or if we
     encounter a region whose initial offset is already past the
     PC we are unwinding too.  */

  while (addr < end && pc_offset > region_offset)
    {
      /* First one must be a record header.  */
      addr = execute_one_ia64_descriptor (addr, frame, &len);
      if (len > 0)
        {
	  region_offset += last_region_size;
	  last_region_size = len;
	}
    }

  /* Now we go get the actual values.  */
  frame_translate (frame, pc_offset);
  if (pc_base_ptr)
    *pc_base_ptr = pc_base;
  return unw_info_ptr;
}

/* Given an unwind info pointer, return the personality routine.  */
void *
__get_personality (unwind_info_ptr *ptr)
{
  void **p;

  /* There is a personality routine only if one of the EHANDLER or UHANDLER
     bits is set.  */
  if (! (IA64_UNW_HDR_FLAGS (ptr->header)
	 & (IA64_UNW_EHANDLER|IA64_UNW_UHANDLER)))
    return 0;

  p = (void **) (ptr->unwind_descriptors
		 + IA64_UNW_HDR_LENGTH (ptr->header) * 8);
  return *p;
}

/* Given an unwind info pointer, return the exception table.  */
void *
__get_except_table (unwind_info_ptr *ptr)
{
  void *table;

  /* If there is no personality, there is no handler data.
     There is a personality routine only if one of the EHANDLER or UHANDLER
     bits is set.  */
  if (! (IA64_UNW_HDR_FLAGS (ptr->header)
	 & (IA64_UNW_EHANDLER|IA64_UNW_UHANDLER)))
    return 0;

  table = (void *) (ptr->unwind_descriptors
		    + IA64_UNW_HDR_LENGTH (ptr->header) * 8 + 8);
  return table;
}

/* Given a PFS value, and the current BSp, calculate the BSp of the caller.  */
void *
__calc_caller_bsp (long pfs, unsigned char *bsp)
{
  int size_of_locals;

  /* The PFS looks like :  xxxx SOL:7 SOF:7. The SOF is bits 0-7 and SOL 
     is bits 8-15. We only care about SOL.  */

  size_of_locals = (pfs >> 7) & 0x7f;
  return rse_address_add (bsp, -size_of_locals);
}

static int 
ia64_backtrace_helper (void **array, ia64_frame_state *throw_frame,
		       ia64_frame_state *frame, void *bsp, void *sp, int size)
{
  void *throw_pc = __builtin_return_address (0);
  void *pc = NULL;
  int frame_count = 0;
  unwind_info_ptr *info;

  __builtin_ia64_flushrs ();      /*  Make the local register stacks available.  */

  /* Start at our stack frame, get our state.  */
  info = __build_ia64_frame_state (throw_pc, throw_frame, bsp, sp, NULL);

  *frame = *throw_frame;

  while (info && frame_count < size)
    {
      pc = array[frame_count++] = __get_real_reg_value (&frame->rp);
      --pc;
      bsp = __calc_caller_bsp 
	((long)__get_real_reg_value (&frame->pfs), frame->my_bsp);
      info = __build_ia64_frame_state (pc, frame, bsp, frame->my_psp, NULL);
      if (frame->rp.loc_type == IA64_UNW_LOC_TYPE_NONE) /* We've finished. */
	break;
    }

  return frame_count;
}

/* This is equivalent to glibc's backtrace(). */

extern int __ia64_backtrace (void **array, int size);
  
int
__ia64_backtrace (void **array, int size)
{
  register void *stack_pointer __asm__("r12");
  ia64_frame_state my_frame;
  ia64_frame_state originator;	/* For the context handler is in.  */
  void *bsp;
 
  /* Do any necessary initialization to access arbitrary stack frames.
     This forces gcc to save memory in our stack frame for saved
     registers. */
  __builtin_unwind_init ();

  bsp = __builtin_ia64_bsp ();
  
  return ia64_backtrace_helper (array, &my_frame, &originator, bsp,
				stack_pointer, size);
}
