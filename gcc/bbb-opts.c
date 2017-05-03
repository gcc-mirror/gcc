/* Bebbo's Optimizations.
 Copyright (C) 2010-2017 Free Software Foundation, Inc.
 Copyright (C) 2017 Stefan "Bebbo" Franke.

 This file is part of GCC.

 GCC is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free
 Software Foundation; either version 3, or (at your option) any later
 version.

 GCC is distributed in the hope that it will be useful, but WITHOUT ANY
 WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

 You should have received a copy of the GNU General Public License
 along with GCC; see the file COPYING3.  If not see
 <http://www.gnu.org/licenses/>.  */

/**
 * SBF (Stefan "Bebbo" Franke):
 *
 * This pass performs multiple optimizations.
 *
 * #1 propagate_moves
 * check if a->b->a can be moved out of a loop.
 *
 * #2 strcpy
 * check if a temp reg can be eliminated.
 *
 * #3 const_comp_sub
 * convert a compare with int constant into sub statement.
 *
 * #4 merge_add
 * merge adds
 *
 * #5 elim_dead_assign
 * eliminate some dead assignments.
 *
 * #6 shrink stack frame
 * remove push/pop for unused variables
 *
 * #7 rename register
 * rename registers without breaking register parameters, inline asm etc.
 *
 * Lessons learned:
 *
 * - do not trust existing code, better delete insns and inster a new one.
 * - do not modify insns, create new insns from pattern
 * - do not reuse registers, create new reg rtx instances
 *
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "cfgrtl.h"
#include "emit-rtl.h"
#include "tree.h"
#include "tree-pass.h"
#include "conditions.h"
#include "langhooks.h"
#include <vector>
#include <set>
#include <map>

bool be_very_verbose;
bool be_verbose;

#ifdef __ECLIPSE__
//extern char * string_bbb_opts;
#define FIRST_PSEUDO_REGISTER 25
#define FRAME_POINTER_REGNUM 13
#define STACK_POINTER_REGNUM 15
#define NOTICE_UPDATE_CC(a,b)
#define Pmode SImode
#endif
extern struct lang_hooks lang_hooks;

/* Lookup of the current function name. */
extern tree current_function_decl;
static tree last_function_decl;
static char const *
get_current_function_name ()
{
  static char fxname[512];
  if (current_function_decl == NULL)
    strcpy (fxname, "<toplevel>");
  else
    strcpy (fxname, lang_hooks.decl_printable_name (current_function_decl, 2));
  return fxname;
}

/* a simple log to stdout. */
static int
log (char const * fmt, ...)
{
  if (!be_verbose)
    return 0;

  va_list args;
  va_start(args, fmt);
  if (last_function_decl != current_function_decl)
    {
      last_function_decl = current_function_decl;
      printf (":bbb: in '%s'\n", get_current_function_name ());
    }
  printf (":bbb: ");
  int retval = vprintf (fmt, args);
  va_end(args);
  fflush (stdout);
  return retval;
}

/* Information for each insn to detect alive registers. Enough for m68k.
 * Why a class? Maybe extend it for general usage.
 *
 * Track use & def separate to determine starting points.
 */
class insn_info
{
  rtx_insn * insn; // the insn

  // usage flags
  unsigned myuse;  // bit set if registers are used in this statement
  unsigned hard; // bit set if registers can't be renamed
  unsigned use;  // bit set if registers are used in program flow
  unsigned def;  // bit set if registers are defined here

  int proepi; // 1 = in prologue, 2 = in epilogue, 0 = other

  bool stack; // part of stack frame insns

  // stuff to analyze insns
  bool label;
  bool jump;
  bool call;
  bool compare;
  bool dst_mem;
  bool src_mem;
  bool dst_plus;
  bool src_plus;
  bool src_const;

  rtx dst_reg;
  rtx dst_mem_reg;
  rtx src_reg;
  rtx src_mem_reg;
  unsigned dst_mem_addr;

  unsigned src_intval;

public:
  insn_info (rtx_insn * i = 0, int p = 0) :
      insn (i), myuse (0), hard (0), use (0), def (0), proepi (p), stack (false), label (false), jump (false), call (
	  false), compare (false), dst_mem (false), src_mem (false), dst_plus (false), src_plus (false), src_const (
	  false), dst_reg (0), dst_mem_reg (0), src_reg (0), src_mem_reg (0), dst_mem_addr (0), src_intval (0)
  {
  }

  int
  get_index () const;

  void
  plus_to_move (rtx_insn * newinsn);

  void
  swap_adds (rtx_insn * newinsn, insn_info & ii);

  void
  absolute2base (unsigned regno, unsigned base);

  inline bool
  is_dst_reg () const
  {
    return dst_reg;
  }

  inline bool
  is_dst_mem () const
  {
    return dst_mem;
  }

  inline bool
  is_src_mem () const
  {
    return src_mem;
  }

  inline bool
  has_dst_memreg () const
  {
    return dst_mem_reg;
  }

  inline bool
  has_dst_addr () const
  {
    return dst_mem_addr;
  }

  inline bool
  is_label () const
  {
    return label;
  }

  inline bool
  is_jump () const
  {
    return jump;
  }

  inline bool
  is_call () const
  {
    return call;
  }

  inline unsigned
  get_dst_addr () const
  {
    return dst_mem_addr;
  }

  inline bool
  is_src_reg () const
  {
    return src_reg && !src_plus;
  }

  inline bool
  is_src_plus () const
  {
    return src_reg && src_plus;
  }

  inline bool
  is_src_mem_plus () const
  {
    return src_mem && src_plus;
  }

  inline int
  get_dst_regno () const
  {
    return dst_reg ? REGNO(dst_reg) : -1;
  }

  inline int
  get_src_regno () const
  {
    return src_reg ? REGNO(src_reg) : -1;
  }

  inline rtx
  get_src_reg () const
  {
    return src_reg;
  }

  inline rtx
  get_dst_reg () const
  {
    return dst_reg;
  }

  inline int
  get_src_mem_regno () const
  {
    return src_mem_reg ? REGNO(src_mem_reg) : -1;
  }

  inline int
  get_src_intval () const
  {
    return src_intval;
  }

  inline bool
  is_src_const () const
  {
    return src_const;
  }

  inline void
  mark_jump ()
  {
    jump = true;
  }
  inline void
  mark_call ()
  {
    call = true;
  }
  inline void
  mark_label ()
  {
    label = true;
  }

  void
  fledder (rtx set);

  /* update usage. */
  void
  update (insn_info & o)
  {
    myuse = o.myuse;
    hard = o.hard;
    use = o.use;
    def = o.def;
  }

  inline rtx_insn *
  get_insn () const
  {
    return insn;
  }

  void
  mark_stack ()
  {
    stack = true;
  }

  bool
  is_stack () const
  {
    return stack;
  }

  inline int
  in_proepi () const
  {
    return proepi;
  }

  inline void
  reset ()
  {
    use = 0;
    def = 0;
    hard = 0;
  }

  inline bool
  is_empty ()
  {
    return !def && !use && !hard;
  }

  inline void
  mark_use (int regno)
  {
    myuse |= 1 << regno;
    use |= 1 << regno;
  }

  inline void
  mark_def (int regno)
  {
    def |= 1 << regno;
  }

  inline void
  mark_hard (int regno)
  {
    hard |= 1 << regno;
  }

  inline void
  unset (int regno)
  {
    use &= ~(1 << regno);
    def &= ~(1 << regno);
    hard &= ~(1 << regno);
  }

  inline unsigned
  get_use () const
  {
    return use;
  }

  inline void
  set_use (unsigned u)
  {
    use = u;
  }

  inline unsigned
  get_def () const
  {
    return def;
  }
  inline unsigned
  get_hard () const
  {
    return hard;
  }

  inline bool
  is_use (int regno)
  {
    return (use & (1 << regno)) != 0;
  }

  inline bool
  is_myuse (int regno)
  {
    return (myuse & (1 << regno)) != 0;
  }

  inline bool
  is_def (int regno)
  {
    return (def & (1 << regno)) != 0;
  }

  inline bool
  is_hard (int regno)
  {
    return (hard & (1 << regno)) != 0;
  }

  inline void
  clear_hard_def ()
  {
    hard = 0;
    def = 0;
  }

  /*
   * update for previous insn.
   * - remove regs which are defined here
   * - add regs which are used here
   * - reset _def
   * - restrain _hard to used
   */
  inline void
  updateWith (insn_info const & o)
  {
    use &= ~o.def;
    use |= o.use;
    def = 0;
  }

  inline insn_info &
  merge (insn_info const & o)
  {
    use = (use & ~o.def) | o.use;
    def |= o.def;
    hard |= o.hard;
    return *this;
  }

  inline insn_info &
  or_use (insn_info const & o)
  {
    use |= o.myuse | o.def | o.hard;
    return *this;
  }

  inline insn_info &
  drop_def ()
  {
    use &= ~def;
    return *this;
  }

#if 0
  inline insn_info
  operator | (insn_info const & o) const
    {
      insn_info t;
      t.use = use | o.use;
      t.def = def | o.def;
      t.hard = hard | o.hard;
      return t;
    }

  inline bool
  operator == (insn_info const & o)
    {
      return use == o.use;
    }

  inline insn_info
  operator ~ () const
    {
      insn_info t;
      t.use = ~use;
      t.def = ~def;
      t.hard = ~hard;
      return t;
    }
#endif

  inline insn_info &
  make_hard ()
  {
    hard = use | def;
    return *this;
  }

  inline insn_info &
  make_clobber ()
  {
    hard = use = def = use | def;
    return *this;
  }

  inline bool
  contains (insn_info const & o) const
  {
    if (o.def & ~def)
      return false;
    if (o.use & ~use)
      return false;
    if (o.hard & ~hard)
      return false;
    return true;
  }

  void
  scan ();

  void
  scan_rtx (rtx);

  /* return bits for alternate free registers. */
  unsigned
  get_free_mask () const
  {
    if (def & hard)
      return 0;

    if (!def || (def & ~(1 << FIRST_PSEUDO_REGISTER)) > 0x1000)
      return 0;

    unsigned mask = def - 1;
    /* more than one register -> don't touch. */
    if ((mask & ~def) != mask)
      return 0;

    if (def > 0xff)
      mask &= 0xff00;

    return mask & ~use;
  }

  unsigned
  get_regbit () const
  {
    return def & ~hard & ~use & 0x7fff;
  }

  void
  set_insn (rtx_insn * newinsn);

};

void
insn_info::scan ()
{
  rtx pattern = PATTERN (insn);
  if (ANY_RETURN_P(pattern))
    {
      tree type = TYPE_SIZE(TREE_TYPE (DECL_RESULT (current_function_decl)));
      int sz = type ? TREE_INT_CST_LOW(type) : 0;
      // log ("return size %d\n", sz);
      if (sz <= 64)
	{
	  mark_hard (0);
	  mark_use (0);
	  if (sz > 32)
	    {
	      mark_hard (1);
	      mark_use (1);
	    }
	}
    }
  else if (CALL_P(insn))
    {
      /* add mregparm registers. */
      for (rtx link = CALL_INSN_FUNCTION_USAGE(insn); link; link = XEXP(link, 1))
	{
	  rtx op, reg;

	  if (GET_CODE (op = XEXP (link, 0)) == USE && REG_P(reg = XEXP (op, 0)))
	    for (unsigned r = REGNO(reg); r <= END_REGNO (reg); ++r)
	      mark_use (r);
	}
      /* mark scratch registers. */
      mark_def (0);
      mark_def (1);
      mark_def (8);
      mark_def (9);
      /* also mark all registers as not renamable */
      hard = use;
    }
  scan_rtx (pattern);
}

/* scan rtx for registers and set the corresponding flags. */
void
insn_info::scan_rtx (rtx x)
{
  if (REG_P(x))
    {
      for (int n = REG_NREGS(x), r = REGNO(x); n > 0; --n, ++r)
	mark_use (r);
      return;
    }

  if (x == cc0_rtx)
    {
      mark_use (FIRST_PSEUDO_REGISTER);
      return;
    }

  RTX_CODE code = GET_CODE(x);

  /* handle SET and record use and def. */
  if (code == SET)
    {
      unsigned u = use;
      unsigned mu = myuse;
      scan_rtx (SET_DEST(x));
      if (REG_P(SET_DEST(x)))
	{
	  def |= use;
	  use = u;
	  myuse = mu;
	}
      scan_rtx (SET_SRC(x));
      int code = GET_CODE(SET_SRC(x));
      if (code == ASM_OPERANDS)
	use = hard |= def | use;
      return;
    }

  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx (XEXP(x, i));
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx (XVECEXP(x, i, j));
    }
}

/* read the set and grab infos */
void
insn_info::fledder (rtx set)
{
  rtx dst = SET_DEST(set);
  rtx src = SET_SRC(set);

  if (dst == cc0_rtx)
    {
      compare = true;
      set = src;
      dst = SET_DEST(set);
      src = SET_SRC(set);
    }

  if (REG_P(dst))
    {
      dst_reg = dst;
    }
  else if (MEM_P(dst))
    {
      dst_mem = true;
      rtx mem = XEXP(dst, 0);
      if (REG_P(mem))
	dst_mem_reg = mem;
      else if (GET_CODE(mem) == CONST_INT)
	dst_mem_addr = INTVAL(mem);
      else if (GET_CODE(mem) == PLUS)
	{
	  dst_plus = true;
	  rtx reg = XEXP(mem, 0);
	  rtx konst = XEXP(mem, 1);
	  if (REG_P(reg) && GET_CODE(konst) == CONST_INT)
	    {
	      dst_mem_reg = reg;
	      dst_mem_addr = INTVAL(konst);
	    }
	}
    }

  if (REG_P(src))
    {
      src_reg = src;
    }
  else if (MEM_P(src))
    {
      src_mem = true;
      rtx mem = XEXP(src, 0);
      if (REG_P(mem))
	src_mem_reg = mem;
      else if (GET_CODE(mem) == CONST_INT)
	src_intval = INTVAL(mem);
      else if (GET_CODE(mem) == PLUS)
	{
	  src_plus = true;
	  rtx reg = XEXP(mem, 0);
	  rtx konst = XEXP(mem, 1);
	  if (REG_P(reg) && GET_CODE(konst) == CONST_INT)
	    {
	      src_mem_reg = reg;
	      src_const = true;
	      src_intval = INTVAL(konst);
	    }
	}
    }
  else if (GET_CODE(src) == CONST_INT)
    {
      src_const = true;
      src_intval = INTVAL(src);
    }
  else if (GET_CODE(src) == PLUS)
    {
      src_plus = true;
      rtx reg = XEXP(src, 0);
      rtx konst = XEXP(src, 1);
      if (REG_P(reg))
	{
	  if (GET_CODE(konst) == CONST_INT)
	    {
	      src_reg = reg;
	      src_const = true;
	      src_intval = INTVAL(konst);
	    }
	  else if (REG_P(konst))
	    {
	      src_reg = konst;
	    }
	}
    }

}

/* create a copy for a reg. Optional specify a new register number. */
static rtx
copy_reg (rtx reg, int newregno)
{
  if (newregno < 0)
    newregno = REGNO(reg);
  rtx x = gen_raw_REG (GET_MODE(reg), newregno);
  x->jump = reg->jump;
  x->call = reg->call;
  x->unchanging = reg->unchanging;
  x->volatil = reg->volatil;
  x->in_struct = reg->in_struct;
  x->used = reg->used;
  x->frame_related = reg->frame_related;
  x->return_val = reg->return_val;

  x->u.reg.attrs = reg->u.reg.attrs;
  return x;
}

/* Rename the register plus track all locs to undo these changes. */
static void
temp_reg_rename (std::vector<std::pair<rtx *, rtx> > & loc, rtx x, unsigned oldregno, unsigned newregno)
{
  if (!x)
    return;

  RTX_CODE code = GET_CODE(x);

  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx y = XEXP(x, i);
	  if (REG_P(y))
	    {
	      if (REGNO(y) == oldregno)
		{
		  rtx z = copy_reg (y, newregno);
		  loc.push_back (std::make_pair (&XEXP(x, i), y));
		  XEXP(x, i) = z;
		}
	    }
	  else
	    temp_reg_rename (loc, y, oldregno, newregno);
	}
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  temp_reg_rename (loc, XVECEXP(x, i, j), oldregno, newregno);
    }
}

/*
 * Collect some data.
 */
static std::vector<insn_info> infos;
static std::vector<rtx_insn *> jumps;
static std::map<rtx_insn *, insn_info *> insn2index;

static insn_info * info0;

static void
update_insn2index ()
{
  infos.reserve (infos.size () * 8 / 7 + 2);
  insn2index.clear ();
  /* needs a separate pass since the insn_infos require fixed addresses for ->get_index() */
  for (unsigned i = 0; i < infos.size (); ++i)
    {
      insn_info & ii = infos[i];
      insn2index.insert (std::make_pair (ii.get_insn (), &ii));
    }
  info0 = &infos[0];
}

int
insn_info::get_index () const
{
  insn_info * ii = &infos[0];

  if (ii == info0)
    {
      ptrdiff_t diff = ((char const *) this - (char const *) ii);
      unsigned pos = diff / sizeof(insn_info);
      if (pos < infos.size ())
	return pos;
    }

  // realloc happened...
  for (unsigned i = 0; i < infos.size (); ++i)
    if (infos[i].get_insn () == this->insn)
      return i;

  // whoops!?
  return 0;
}

void
insn_info::plus_to_move (rtx_insn * newinsn)
{
  insn = newinsn;
  src_plus = false;
  src_reg = XEXP(PATTERN (newinsn), 1);
  insn2index.insert (std::make_pair (insn, this));
  // usage flags did not change
}

void
insn_info::swap_adds (rtx_insn * newinsn, insn_info & ii)
{
  insn = newinsn;

  std::swap (*this, ii);

  insn2index.insert (std::make_pair (insn, this));
  insn2index.insert (std::make_pair (ii.insn, &ii));

  // usage flags did not change
}

void
insn_info::set_insn (rtx_insn * newinsn)
{
  insn = newinsn;
  fledder (PATTERN (insn));
}

void
insn_info::absolute2base (unsigned regno, unsigned base)
{
  rtx set = PATTERN (get_insn ());
  rtx src = SET_SRC(set);
  machine_mode mode = GET_MODE(SET_DEST(set));

  unsigned addr = get_dst_addr ();
  unsigned offset = addr - base;

  rtx pattern;
  rtx reg = gen_raw_REG (SImode, regno);
  if (base == addr)
    pattern = gen_rtx_SET(gen_rtx_MEM (mode, reg), src);
  else
    pattern = gen_rtx_SET(gen_rtx_MEM (mode, gen_rtx_PLUS(SImode, reg, gen_rtx_CONST_INT(SImode, offset))), src);

  SET_INSN_DELETED(insn);
  insn = emit_insn_after (pattern, insn);

  mark_use (regno);

  dst_mem_reg = reg;
  dst_mem = true;
  dst_mem_addr = offset;
  dst_plus = offset != 0;

  insn2index.insert (std::make_pair (insn, this));
}
/*
 * Reset collected data.
 */
static void
clear (void)
{
  jumps.clear ();
  insn2index.clear ();
  infos.clear ();
}

/*
 *  return true if the register is DEAD.
 *  Do not check at jumps.
 */
static bool
is_reg_dead (unsigned regno, unsigned _pos)
{
  // skip labels.
  for (unsigned pos = _pos + 1; pos < infos.size (); ++pos)
    {
      insn_info & ii0 = infos[pos];
      // skip entries without info
      if (ii0.is_empty ())
	continue;

      // not dead if usage is reported in the next statement
      return !ii0.is_use (regno) && !ii0.is_hard (regno);
    }
  return true;
}

/* helper stuff to enhance the asm output. */
int my_flag_regusage;
void
append_reg_usage (FILE * f, rtx_insn * insn)
{

  auto i = insn2index.find (insn);
  if (i == insn2index.end ())
    return;

  insn_info & ii = *i->second;

  if (f != stderr)
    fprintf (f, "\n\t\t\t\t\t\t|%c ", ii.is_stack () ? 's' : ' ');

  for (int j = 0; j < 8; ++j)
    if (ii.is_use (j) || ii.is_def (j))
      {
	fprintf (f, ii.is_hard (j) ? "!" : " ");
	fprintf (f, ii.is_def (j) ? ii.is_use (j) ? "*" : "+" : " ");
	fprintf (f, "d%d ", j);
      }
    else
      fprintf (f, "     ");

  for (int j = 8; j < 16; ++j)
    if (ii.is_use (j) || ii.is_def (j))
      {
	fprintf (f, ii.is_hard (j) ? "!" : " ");
	fprintf (f, ii.is_def (j) ? ii.is_use (j) ? "*" : "+" : " ");
	fprintf (f, "a%d ", j - 8);
      }
    else
      fprintf (f, "     ");

  if (ii.is_use (FIRST_PSEUDO_REGISTER))
    fprintf (f, ii.is_def (FIRST_PSEUDO_REGISTER) ? "+cc " : " cc ");

  if (f == stderr)
    fprintf (f, "\n");

}

/*
 * Helper function to dump the code.
 * Sometimes used during debugging.
 */
static void
dump_insns (char const * name, bool all)
{
  fprintf (stderr, "====================================: %s\n", name);
  if (all)
    {
      for (rtx_insn * insn = get_insns (); insn && insn != infos[0].get_insn (); insn = NEXT_INSN (insn))
	debug_rtx (insn);
    }
  for (unsigned i = 0; i < infos.size (); ++i)
    {
      fprintf (stderr, "%d: ", i);

      rtx_insn * insn = infos[i].get_insn ();
      if (i < infos.size ())
	append_reg_usage (stderr, insn);

      fprintf (stderr, "\t");
      debug_rtx (insn);

      if (all)
	{
	  rtx_insn * p = i + 1 < infos.size () ? infos[i + 1].get_insn () : 0;
	  for (rtx_insn * q = NEXT_INSN (insn); q && q != p; q = NEXT_INSN (q))
	    debug_rtx (q);
	}
    }
}

/* This is the important function to track register usage plus hard/live state.
 *
 * Start at bottom and work upwards. On all labels trigger all jumps referring to this label.
 * A set destination into a register is a def. All other register references are an use.
 * Hard registers cann't be renamed and are mandatory for regparms and asm_operands.
 */
static void
update_insn_infos (void)
{
  /* own analyze reg life */
  std::vector<std::pair<unsigned, insn_info> > todo;
  todo.push_back (std::make_pair (infos.size () - 1, insn_info ()));

  int pass = 0;
  while (!todo.empty ())
    {
      std::pair<unsigned, insn_info> p = *todo.rbegin ();
      todo.pop_back ();

      insn_info ii = p.second;

      for (int pos = p.first; pos >= 0; --pos)
	{
	  rtx_insn * insn = infos[pos].get_insn ();
	  /* can be NULL as used in opt_shrink_stack_frame(). */
	  if (!insn)
	    continue;

	  /* no new information -> break. */
	  if (pass && infos[pos].contains (ii))
	    break;

	  ii.clear_hard_def ();
	  ii.merge (infos[pos]);

	  if (LABEL_P(insn))
	    {
	      /* work on all jumps referring to that label. */
	      for (auto i = jumps.begin (); i != jumps.end (); ++i)
		{
		  if (JUMP_LABEL(*i) == insn)
		    {
		      auto j = insn2index.find (*i);
		      if (j != insn2index.end ())
			todo.push_back (std::make_pair (j->second->get_index (), ii));
		    }
		}
	      continue;
	    }

	  rtx pattern = PATTERN (insn);
	  insn_info use (insn);
	  use.scan ();

	  if (CALL_P(insn))
	    {
	    }
	  else if (JUMP_P(insn))
	    {
	      if (ANY_RETURN_P(pattern))
		{
		  ii.reset ();
		}
	    }
	  else if (GET_CODE (pattern) == USE || GET_CODE (pattern) == CLOBBER)
	    {
	      use.make_clobber ();
	    }
	  else if (single_set (insn) == 0)
	    use.make_hard ();
	  else
	  /* if not cc0 defined check for mod. */
	  if (!use.is_def (FIRST_PSEUDO_REGISTER))
	    {
	      CC_STATUS_INIT;
	      NOTICE_UPDATE_CC (PATTERN (insn), insn);
	      if (cc_status.value1 || cc_status.value2)
		use.mark_def (FIRST_PSEUDO_REGISTER);
	    }

	  /* mark not renameable in prologue/epilogue. */
	  if (infos[pos].in_proepi ())
	    use.make_hard ();

	  ii.merge (use);
	  infos[pos].update (ii);
	  ii.updateWith (use);
	}
      ++pass;
    }
}

/*
 * Create a filtered view of insns - keep only those to work with.
 */
static void
update_insns ()
{
  rtx_insn *insn, *next;
  clear ();

  char inproepilogue = 1;
  /* create a vector with relevant insn. */
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (NONJUMP_INSN_P (insn) || LABEL_P(insn) || JUMP_P(insn) || CALL_P(insn))
	{

	  infos.push_back (insn_info (insn, inproepilogue));
	  insn_info & ii = infos[infos.size () - 1];

	  if (JUMP_P(insn))
	    {
	      jumps.push_back (insn);
	      inproepilogue = 0;

	      ii.mark_jump ();
	    }
	  else if (LABEL_P(insn))
	    {
	      ii.mark_label ();
	    }
	  else if (CALL_P(insn))
	    {
	      ii.mark_call ();
	    }
	  else
	    {
	      rtx set = single_set (insn);
	      if (set)
		ii.fledder (set);
	    }
	}

      if (NOTE_P(insn))
	{
	  if (NOTE_KIND(insn) == NOTE_INSN_PROLOGUE_END)
	    inproepilogue = 0;
	  else if (NOTE_KIND(insn) == NOTE_INSN_EPILOGUE_BEG)
	    inproepilogue = 2;
	}
    }

  update_insn2index ();
  update_insn_infos ();
}

/* convert the lowest set bit into a register number. */
static int
bit2regno (unsigned bit)
{
  if (!bit)
    return -1;

  unsigned regno = 0;
  while (!(bit & 1))
    {
      ++regno;
      bit >>= 1;
    }
  return regno;
}

/* check if that register is touched between from and to, excluding from and to .*/
static bool
is_reg_touched_between (unsigned regno, int from, int to)
{
  for (int index = from + 1; index < to; ++index)
    {
      insn_info & ii = infos[index];
      if (ii.is_myuse (regno) || ii.is_def (regno))
	return true;
    }
  return false;
}

/*
 * search backward and find the initial assignment for that regno.
 */
static unsigned
find_start (std::set<unsigned> & found, unsigned start, unsigned rename_regno)
{
  /* search the start. */
  while (start > 0)
    {
      unsigned startm1 = start - 1;

      /* already searched. */
      if (found.find (startm1) != found.end ())
	break;

      /* do not run over RETURNS */
      rtx_insn * before = infos[startm1].get_insn ();
      if (JUMP_P(before) && ANY_RETURN_P(PATTERN (before)))
	break;

      start = startm1;

      /* found the definition without use. */
      insn_info & jj = infos[start];
      if (jj.is_def (rename_regno) && !jj.is_use (rename_regno))
	break;

    }
  return start;
}

/*
 * Always prefer lower register numbers within the class.
 */
static unsigned
opt_reg_rename (void)
{
//  dump_insns ("rename", 1);
  for (unsigned index = 0; index < infos.size (); ++index)
    {
      insn_info & ii = infos[index];

      /* do not rename if register is hard or used in same statement. */
      const unsigned rename_regbit = ii.get_regbit ();
      if (!rename_regbit)
	continue;

      const unsigned rename_regno = bit2regno (rename_regbit);

      /* get the mask for free registers. */
      unsigned mask = ii.get_free_mask ();
      if (!mask)
	continue;

      /* first = pos to start, second indicates to treat def as use. */
      std::vector<unsigned> todo;
      std::set<unsigned> found;
      if (index + 1 < infos.size ())
	todo.push_back (index + 1);

      found.insert (index);
      /* a register was defined, follow all branches. */
      while (mask && todo.size ())
	{
	  unsigned runpos = todo[todo.size () - 1];
	  todo.pop_back ();

	  for (unsigned pos = runpos; mask && pos < infos.size (); ++pos)
	    {
	      /* already searched. */
	      if (found.find (pos) != found.end ())
		break;

	      rtx_insn * insn = infos[pos].get_insn ();
	      if (LABEL_P(insn))
		{
		  found.insert (pos);

		  /* for each jump to this label:
		   * check if the reg was used at that jump.
		   * if used, find def
		   */
		  for (auto i = jumps.begin (); i != jumps.end (); ++i)
		    {
		      if (JUMP_LABEL(*i) == insn)
			{
			  auto j = insn2index.find (*i);
			  if (j == insn2index.end ())
			    continue;

			  unsigned start = j->second->get_index ();
			  if (!infos[start].is_use (rename_regno))
			    continue;

			  start = find_start (found, start, rename_regno);
			  todo.push_back (start);
			}
		    }
		  continue;
		}

	      insn_info & jj = infos[pos];

	      /* marked as hard reg -> invalid rename */
	      if (jj.get_use () & jj.get_hard () & rename_regbit)
		{
		  mask = 0;
		  break;
		}

	      /* not used. and not a def */
	      if (pos == runpos && (jj.get_def () & rename_regbit))
		{
		  /* continue since this pos was added by start search. */
		}
	      else if (!(jj.get_use () & rename_regbit))
		break;

	      /* update free regs. */
	      mask &= ~jj.get_use ();
	      mask &= ~jj.get_def ();
	      if (!mask)
		break;

	      found.insert (pos);

	      /* follow jump and/or next insn. */
	      if (JUMP_P(insn))
		{
		  auto j = insn2index.find ((rtx_insn *) JUMP_LABEL(insn));
		  if (j == insn2index.end ())
		    {
		      /* whoops - label not found. */
		      mask = 0;
		      break;
		    }

		  unsigned label_index = j->second->get_index ();
		  if (found.find (label_index) == found.end ())
		    {
		      /* if the rename_reg is used in the insn before.
		       * search the start.
		       */
		      insn_info & bb = infos[label_index + 1];
		      if (bb.is_use (rename_regno))
			{
			  unsigned start = find_start (found, label_index, rename_regno);
			  todo.push_back (start);
			}
		      todo.push_back (label_index + 1);
		    }
		  rtx jmppattern = PATTERN (insn);
		  if (GET_CODE(jmppattern) == PARALLEL)
		    {
		      /* can't handle yet. Abort renaming. */
		      mask = 0;
		      break;
		    }

		  rtx jmpsrc = XEXP(jmppattern, 1);
		  if (!jmpsrc || GET_CODE(jmpsrc) != IF_THEN_ELSE)
		    break;
		}
	    }
	}

      if (mask)
	{
	  int oldregno = bit2regno (rename_regbit);
	  int newregno = bit2regno (mask);

	  /* check the renamed insns. */
	  std::vector<std::pair<rtx *, rtx> > locs;
	  std::vector<std::pair<rtx *, rtx> > patch;
	  bool ok = true;

	  for (std::set<unsigned>::iterator i = found.begin (); ok && i != found.end (); ++i)
	    {
	      rtx_insn * insn = infos[*i].get_insn ();

	      /* temp rename. */
	      temp_reg_rename (locs, PATTERN (insn), oldregno, newregno);
	      if (!locs.empty ())
		{
		  int num_clobbers_to_add = 0;
		  int insn_code_number = recog (PATTERN (insn), insn, &num_clobbers_to_add);
		  if (insn_code_number < 0 || !check_asm_operands (PATTERN (insn)))
		    ok = false;

		  /* undo temp change but keep loc and new register. */
		  for (std::vector<std::pair<rtx *, rtx> >::iterator j = locs.begin (); j != locs.end (); ++j)
		    {
		      patch.push_back (std::make_pair (j->first, *j->first));
		      *j->first = j->second;
		    }

		  locs.clear ();
		}
	    }

	  if (!ok)
	    continue;

	  log ("opt_reg_rename %s -> %s (%d locs, start at %d)\n", reg_names[oldregno], reg_names[newregno],
	       patch.size (), index);

	  /* apply all changes. */
	  for (std::vector<std::pair<rtx *, rtx> >::iterator j = patch.begin (); j != patch.end (); ++j)
	    *j->first = j->second;

	  return 1;
	}
    }
  return 0;
}

/*
 *  #1 propagate a->b->a moves out of a loop.
 *
 * consider a loop:
 *
 * .L1
 *   ...
 *   move d0, a0 ; (1)
 *   ...
 *   move xy, (a0)+
 *   ...
 *   move a0, d0 ; (2)
 *   ...
 *   jxx .L1
 *
 *  Then the statements (1) and (2) can be moved out of the loop:
 *
 *   move d0, a0 ; (3)
 * .L1
 *   ...
 *   move *, (a0)+ ; a0 is modified somehow
 *   ...
 *   jxx .L1
 *   move a0, d0 ; (4)
 *
 *  if all criteria are met:
 *
 *  a) no other jump to .L1 -> (LABEL_NUSES(insn) == 1)
 *  b) no other use of d0 inside the loop
 *  c) no other use of a0 before (1)
 *  d) no other use of a1 after (2)
 *
 *  Optional:
 *  - omit (4) if d0 is dead
 *
 *  this will e.g. convert
 .L6:
 move.l d0,a1
 move.b (a1)+,d1
 move.l a1,d0
 move.b d1,(a0)+
 cmp.b #0, d1
 jne .L6
 *  to
 move.l d0,a1
 .L6:
 move.b (a1)+,d1
 move.b d1,(a0)+
 cmp.b #0, d1
 jne .L6

 *
 * Also allow exit jumps, if the modification of the reg is const
 * and insert a correction after the exit label.
 * The label must only be reachable by the exit jump.
 */
static unsigned
opt_propagate_moves ()
{
  unsigned change_count = 0;
  rtx_insn * current_label = 0;
  unsigned current_label_index;
  std::vector<unsigned> reg_reg;
  std::vector<rtx_insn *> jump_out;

  /* start at 1 since there must be an insn before the label. */
  for (unsigned index = 1; index < infos.size (); ++index)
    {
      rtx_insn * insn = infos[index].get_insn ();

      if (LABEL_P(insn))
	{
	  if (LABEL_NUSES(insn) == 1)
	    {
	      current_label = insn;
	      current_label_index = index;
	      reg_reg.clear ();
	      jump_out.clear ();
	    }
	  else
	    current_label = 0;
	}

      if (current_label == 0)
	continue;

      if (NONJUMP_INSN_P(insn))
	{
	  // check for set reg, reg
	  rtx set = single_set (insn);
	  if (set)
	    {
	      rtx src = SET_SRC(set);
	      rtx dst = SET_DEST(set);
	      if (REG_P(src) && REG_P(dst))
		reg_reg.push_back (index);
	    }
	  else
	    current_label = 0;

	  continue;
	}

      if (JUMP_P(insn))
	{
	  rtx_insn * label = (rtx_insn *) JUMP_LABEL(insn);
	  if (label != current_label)
	    {
	      /* collect the labels for a later check if a fixup is possible. */
	      if (LABEL_NUSES(label) == 1 && BARRIER_P(PREV_INSN (label)))
		jump_out.push_back (label);
	      else
		current_label = 0;
	      continue;
	    }

	  if (reg_reg.size () > 1)
	    {
	      /* Search for reg/reg pairs. */
	      for (std::vector<unsigned>::iterator i = reg_reg.begin (); i != reg_reg.end () && i + 1 != reg_reg.end ();
		  )
		{
		  bool inc = true;
		  for (std::vector<unsigned>::iterator j = i + 1; j != reg_reg.end ();)
		    {
		      rtx_insn * ii = infos[*i].get_insn ();
		      rtx seti = single_set (ii);
		      rtx srci = SET_SRC(seti);
		      rtx dsti = SET_DEST(seti);
		      rtx_insn * jj = infos[*j].get_insn ();
		      rtx setj = single_set (jj);
		      rtx srcj = SET_SRC(setj);
		      rtx dstj = SET_DEST(setj);

		      if (rtx_equal_p (srci, dstj) && rtx_equal_p (srcj, dsti))
			{
			  /* Ensure correct usage. */
			  if (is_reg_touched_between (REGNO(srci), current_label_index, *i) // label ... move src,x
			  || is_reg_touched_between (REGNO(srci), *i, *j) // move src,x ... move x,src
			      || is_reg_touched_between (REGNO(srci), *j, index) // move x,src ... jcc
			      || is_reg_touched_between (REGNO(dsti), *j, index) // label ... move src,x
			      || is_reg_touched_between (REGNO(dsti), *j, index) // move x,src ... jcc
							 )
			    {
			      ++j;
			      continue;
			    }

			  std::vector<int> fixups;

			  /* if there are jumps out of the loop,
			   * check if the modification occurs before the jump,
			   * and if, that it's a plus const.
			   */
			  if (jump_out.size ())
			    {
			      std::vector<rtx_insn *>::iterator label_iter = jump_out.begin ();
			      int fixup = 0;

			      for (unsigned k = *i + 1; k != *j; ++k)
				{
				  rtx_insn * check = infos[k].get_insn ();
				  if (JUMP_P(check))
				    {
				      fixups.push_back (fixup);
				      if (++label_iter == jump_out.end ())
					break;
				      continue;
				    }

				  if (reg_overlap_mentioned_p (dsti, PATTERN (check)))
				    {
				      /* right now only support auto_incs. */
				      rtx set = single_set (check);
				      rtx src = SET_SRC(set);
				      rtx dst = SET_DEST(set);

				      if (reg_overlap_mentioned_p (dsti, dst))
					{
					  if (REG_P(dst))
					    break;
					  if (!MEM_P(dst))
					    break;

					  rtx x = XEXP(dst, 0);
					  if (GET_CODE(x) == REG)
					    fixup += 0; // direct use
					  else if (GET_CODE(x) == PRE_INC ||
					  GET_CODE(x) == POST_INC)
					    fixup -= GET_MODE_SIZE(GET_MODE(dst));
					  else if (GET_CODE(dst) == PRE_DEC ||
					  GET_CODE(dst) == POST_DEC)
					    fixup += GET_MODE_SIZE(GET_MODE(dst));
					  else
					    break;
					}

				      if (reg_overlap_mentioned_p (dsti, src))
					{
					  if (REG_P(src))
					    fixup += 0;
					  else
					    {
					      if (!MEM_P(src))
						break;

					      rtx x = XEXP(src, 0);
					      if (GET_CODE(x) == REG)
						fixup += 0; // direct use
					      else if (GET_CODE(x) == PRE_INC ||
					      GET_CODE(x) == POST_INC)
						fixup -= GET_MODE_SIZE(GET_MODE(dst));
					      else if (GET_CODE(dst) == PRE_DEC ||
					      GET_CODE(dst) == POST_DEC)
						fixup += GET_MODE_SIZE(GET_MODE(dst));
					      else
						break;
					    }
					}
				    }
				}
			    }

			  /* got a fixup for all jump_outs? */
			  if (fixups.size () == jump_out.size ())
			    {
			      rtx_insn * before = infos[current_label_index - 1].get_insn ();
			      rtx_insn * after = infos[index + 1].get_insn ();
			      rtx bset = single_set (before);

			      log ("propagate_moves condition met, moving regs %s, %s\n",
			      reg_names[REGNO(srci)],
				   reg_names[REGNO(dsti)]);

			      /* Move in front of loop and mark as dead. */
			      rtx_insn * newii = make_insn_raw (PATTERN (ii));
			      SET_INSN_DELETED(ii);

			      /* Plus check if the reg was just loaded. */
			      if (bset)
				{
				  rtx bdst = SET_DEST(bset);
				  if (REG_P(bdst) && REGNO(bdst) == REGNO(srci))
				    {
				      SET_SRC(PATTERN(newii)) = SET_SRC(bset);
//					  SET_INSN_DELETED(ii);
				    }
				}
			      else
				add_reg_note (newii, REG_DEAD, srci);

			      add_insn_after (newii, before, 0);

			      /* Move behind loop - into next BB. */
			      rtx_insn * newjj = make_insn_raw (PATTERN (jj));
			      add_insn_before (newjj, after, 0);
			      SET_INSN_DELETED(jj);

			      reg_reg.erase (j);
			      reg_reg.erase (i);
			      j = reg_reg.end ();
			      inc = false;

			      /* add fixes if there were jumps out of the loop. */
			      if (jump_out.size ())
				{
				  log ("propagate_moves fixing %d jump outs\n", jump_out.size ());

				  for (unsigned k = 0; k < jump_out.size (); ++k)
				    {
				      rtx neu = gen_rtx_SET(
					  dstj, gen_rtx_PLUS(Pmode, dsti, gen_rtx_CONST_INT(Pmode, fixups[k])));
				      emit_insn_after (neu, jump_out[k]);
				    }
				}
			      ++change_count;
			    }
			}
		      if (inc)
			++j;
		    }
		  if (inc)
		    ++i;
		}
	    }
	  current_label = 0;
	}
    }
  return change_count;
}

/**
 * Search for
 *
 *   mov x,reg
 *   mov reg,x
 *   cmp #0, reg
 *   jxx
 *
 * patterns.
 *
 * Use a simple state machine to find the patterns.
 */
static unsigned
opt_strcpy ()
{
  unsigned change_count = 0;
#if HAVE_cc0
  rtx_insn * x2reg = 0;
  rtx_insn * reg2x;
  unsigned int regno;

  for (unsigned index = 0; index < infos.size (); ++index)
    {
      rtx_insn * insn = infos[index].get_insn ();

      if (!NONJUMP_INSN_P(insn))
	{
	  x2reg = 0;
	  continue;
	}

      rtx set = single_set (insn);
      if (!set)
	{
	  x2reg = 0;
	  continue;
	}

      if (x2reg && reg2x)
	{
	  rtx src = SET_SRC(set);
	  if (GET_CODE(src) == COMPARE)
	    {
	      rtx dst = XEXP(src, 0);
	      src = XEXP(src, 1);

//	      if (CONST_INT_P(src) && INTVAL(src) == 0 && find_reg_note (insn, REG_DEAD, dst))
	      if (REG_P(dst) && CONST_INT_P(src) && INTVAL(src) == 0 && is_reg_dead (REGNO(dst), index))
		{
		  /* now check via NOTICE_UPDATE_CC*/
		  NOTICE_UPDATE_CC (PATTERN (reg2x), reg2x);
		  if (cc_status.flags == 0 && rtx_equal_p (dst, cc_status.value2))
		    {
		      int num_clobbers_to_add = 0;
		      int insn_code_number;

		      rtx_insn * newinsn = make_insn_raw (
			  gen_rtx_SET(SET_DEST(single_set(reg2x)), SET_SRC(single_set (x2reg))));
		      insn_code_number = recog (PATTERN (newinsn), newinsn, &num_clobbers_to_add);
		      if (insn_code_number >= 0 && check_asm_operands (PATTERN (newinsn)))
			{
			  rtx link;

			  log ("opt_strcpy condition met, removing compare and joining insns - omit reg %s\n",
			  reg_names[REGNO(dst)]);

			  SET_SRC(single_set(reg2x)) = SET_SRC(single_set (x2reg));

			  for (link = REG_NOTES(x2reg); link; link = XEXP(link, 1))
			    if (REG_NOTE_KIND (link) != REG_LABEL_OPERAND)
			      {
				if (GET_CODE (link) == EXPR_LIST)
				  add_reg_note (reg2x, REG_NOTE_KIND(link), copy_insn_1 (XEXP(link, 0)));
				else
				  add_shallow_copy_of_reg_note (reg2x, link);
			      }

			  SET_INSN_DELETED(x2reg);
			  SET_INSN_DELETED(insn);

			  ++change_count;
			}
		    }
		}
	      x2reg = 0;
	      continue;
	    }
	  reg2x = 0;
	}

      /* check for reg2x first, maybe fallback to x2reg. */
      if (x2reg && reg2x == 0)
	{
	  if (REG_P(SET_SRC(set)) && REGNO(SET_SRC(set)) == regno)
	    {
	      reg2x = insn;
	      continue;
	    }
	  x2reg = 0;
	}

      /* check for a match for x2reg. */
      if (x2reg == 0)
	{
	  if (REG_P(SET_DEST(set)))
	    {
	      x2reg = insn;
	      reg2x = 0;
	      regno = REGNO(SET_DEST(set));
	    }
	}
    }
#endif
  return change_count;
}

/*
 * convert
 *
 * set reg1, plus (reg2, const)
 * set mem(reg2), y
 *
 * ->
 * set reg1, reg2
 * set mem(reg1+), y
 *
 * if size of postinc == const
 *
 (insn 33 32 35 4 (set (reg/v/f:SI 8 a0 [orig:47 s ] [47])
 (plus:SI (reg/v/f:SI 9 a1 [orig:46 s ] [46])
 (const_int 1 [0x1]))) sn.c:5 141 {*addsi3_internal}
 (nil))
 (insn 36 35 37 4 (set (mem:QI (reg/v/f:SI 9 a1 [orig:46 s ] [46]) [0 MEM[base: s_17, offset: 4294967295B]+0 S1 A8])
 (mem:QI (post_inc:SI (reg/v/f:SI 10 a2 [orig:53 s2 ] [53])) [0 MEM[base: s2_19, offset: 4294967295B]+0 S1 A8])) sn.c:5 46 {*m68k.md:1083}
 (expr_list:REG_INC (reg/v/f:SI 10 a2 [orig:53 s2 ] [53])
 (nil)))
 */
static unsigned
opt_commute_add_move (void)
{
  unsigned change_count = 0;

  for (unsigned index = 0; index + 1 < infos.size (); ++index)
    {
      rtx_insn * insn = infos[index].get_insn ();
      rtx set = single_set (insn);
      if (!set)
	continue;

      rtx reg1dst = SET_DEST(set);
      if (!REG_P(reg1dst))
	continue;

      rtx plus = SET_SRC(set);
      if (GET_CODE(plus) != PLUS)
	continue;

      rtx reg1src = XEXP(plus, 0);
      if (!REG_P(reg1src) || reg1src == reg1dst)
	continue;

      rtx cnst = XEXP(plus, 1);
      if (!CONST_INT_P(cnst))
	continue;

      rtx_insn * next = infos[index + 1].get_insn ();
      rtx set2 = single_set (next);
      if (!set2)
	continue;

      rtx dst = SET_DEST(set2);
      if (!MEM_P(dst) || GET_MODE_SIZE(GET_MODE(dst)) != INTVAL(cnst))
	continue;

      rtx memreg = XEXP(dst, 0);
      if (!REG_P(memreg) || REGNO(memreg) != REGNO(reg1src))
	continue;

      rtx pinc = gen_rtx_POST_INC(GET_MODE(dst), reg1dst);
      rtx newmem = replace_equiv_address_nv (dst, pinc);

      rtx_insn * newinsn = make_insn_raw (gen_rtx_SET(reg1dst, reg1src));
      if (recog (PATTERN (newinsn), newinsn, 0) < 0 || !check_asm_operands (PATTERN (newinsn)))
	continue;

      if (validate_change (next, &SET_DEST(set2), newmem, 0))
	{
	  log ("commute_add_move found\n");

	  SET_INSN_DELETED(insn);

	  insn = emit_insn_before (PATTERN (newinsn), next);

	  add_reg_note (next, REG_INC, reg1dst);

	  ++change_count;
	}
    }
  return change_count;
}

/*
 * Replace
 *
 * move x,dx
 * cmp  dx,dy
 *
 * if dx and dy are both dead after compare.
 *
 * with
 *
 * sub #n,dx
 *
 d0 d1 d2 a0 a1 a7       (insn 99 59 41 7 (set (reg:SI 2 d2)
 (const_int 1 [0x1])) sn.c:8 38 {*movsi_m68k}
 (nil))
 d0 d1 d2 a0 a1 a7       (insn 41 99 42 7 (set (cc0)
 (compare (reg/v:SI 1 d1 [orig:54 n ] [54])
 (reg:SI 2 d2))) sn.c:8 16 {*m68k.md:499}
 (expr_list:REG_DEAD (reg:SI 2 d2)
 (expr_list:REG_DEAD (reg/v:SI 1 d1 [orig:54 n ] [54])
 (nil))))
 *
 */
static unsigned
opt_const_cmp_to_sub (void)
{
  unsigned change_count = 0;
#if HAVE_cc0
  for (int index = infos.size () - 2; index > 0; --index)
    {
      rtx_insn * insn = infos[index].get_insn ();
      rtx seti = single_set (insn);
      if (!seti)
	continue;

      rtx dsti = SET_DEST(seti);
      if (dsti != cc0_rtx)
	continue;

      rtx srci = SET_SRC(seti);
      if (GET_CODE(srci) != COMPARE)
	continue;

      rtx left = XEXP(srci, 0);
      rtx right = XEXP(srci, 1);
      if (!REG_P(left) || !REG_P(right) || REG_NREGS(left) > 1 || REG_NREGS(right) > 1)
	continue;

      // TODO
      // FEATURE: check if the next uses are also a add/sub
      // then maybe that add/sub can be adjusted too

//      if (!find_reg_note (insn, REG_DEAD, left) || !find_reg_note (insn, REG_DEAD, right))
//      	continue;
      /* use own reg_dead - reg_notes seem to be inaccurate!? */
      if (!is_reg_dead (REGNO(left), index) || !is_reg_dead (REGNO(right), index))
	continue;

      // maybe add a search?
      rtx_insn * prev = infos[index - 1].get_insn ();
      rtx setp = single_set (prev);
      if (!setp)
	continue;

      rtx constant_reg = SET_DEST(setp);
      if (!REG_P(constant_reg))
	continue;

      rtx srcp = SET_SRC(setp);
      if (!CONST_INT_P(srcp))
	continue;

      int intval = -INTVAL(srcp);
      if (intval < -8 || intval > 7 || intval == 0)
	continue;

      enum machine_mode mode = GET_MODE(constant_reg);
      if (GET_MODE_SIZE(mode) > 4)
	continue;

      //     printf("mode size: %d\n", GET_MODE_SIZE(mode));

      rtx reg = constant_reg == left ? right : constant_reg == right ? left : 0;

      // no gain with address regs.
      if (!reg || REGNO(reg) > 7)
	continue;

      // search the jump(s)
      bool ok = true;
	{
	  // invert all conditions using this statement.
	  std::vector<unsigned> todo;
	  std::vector<unsigned> done;
	  done.resize (infos.size ());
	  todo.push_back (index + 1);

	  while (ok && todo.size ())
	    {
	      unsigned pos = todo[todo.size () - 1];
	      todo.pop_back ();

	      if (done[pos])
		continue;

	      done[pos] = 1;

	      if (infos[pos].is_def (FIRST_PSEUDO_REGISTER))
		continue;

	      if (pos + 1 < infos.size ())
		todo.push_back (pos + 1);

	      rtx_insn * patchme = infos[pos].get_insn ();
	      if (!JUMP_P(patchme))
		continue;

	      auto j = insn2index.find ((rtx_insn *) JUMP_LABEL(patchme));
	      if (j != insn2index.end ())
		todo.push_back (j->second->get_index ());

	      rtx jmppattern = PATTERN (patchme);

	      rtx jmpsrc = XEXP(jmppattern, 1);
	      if (!jmpsrc)
		ok = false;
	      else if (GET_CODE(jmpsrc) == IF_THEN_ELSE)
		{
		  rtx condition = XEXP(jmpsrc, 0);
		  RTX_CODE code = GET_CODE(condition);
		  ok = code == EQ || code == NE;
		}
	    }
	}
      if (!ok)
	continue;

      rtx plus = gen_rtx_PLUS(mode, copy_reg (reg, -1), gen_rtx_CONST_INT (mode, intval));

      rtx_insn * neuprev = make_insn_raw (gen_rtx_SET(copy_reg (reg, -1), plus));

      int num_clobbers_to_add = 0;
      int insn_code_number = recog (PATTERN (neuprev), neuprev, &num_clobbers_to_add);
      if (insn_code_number < 0 || !check_asm_operands (PATTERN (neuprev)))
	continue;

      // also convert current statement to cmp #0, reg
      SET_INSN_DELETED(insn);
      rtx copyreg = copy_reg (reg, -1);
      rtx neu = gen_rtx_SET(cc0_rtx, gen_rtx_COMPARE(mode, copyreg, gen_rtx_CONST_INT(mode, 0)));
      insn = emit_insn_after (neu, prev);
      add_reg_note (insn, REG_DEAD, copyreg);

      SET_INSN_DELETED(prev);
      prev = emit_insn_before (PATTERN (neuprev), insn);

      log ("const_cmp_to_sub replaced reg-reg compare with sub\n");

      ++change_count;
    }
#endif
  return change_count;
}

/*
 * Some optimizations (e.g. propagate_moves) might result into an unused assignment behind the loop.
 * delete those insns.
 */
static unsigned
opt_elim_dead_assign (void)
{
  unsigned change_count = 0;
  for (int index = infos.size () - 1; index >= 0; --index)
    {
      rtx_insn * insn = infos[index].get_insn ();
      if (!NONJUMP_INSN_P(insn))
	continue;

      rtx set = single_set (insn);
      if (!set)
	continue;

      rtx src = SET_SRC(set);
      rtx dst = SET_DEST(set);
      if (!REG_P(dst) || !REG_P(src))
	continue;

      if (is_reg_dead (REGNO(dst), index))
	{
	  log ("%d: elim_dead_assign to %s\n", index, reg_names[REGNO(dst)]);
	  SET_INSN_DELETED(insn);
	  ++change_count;
	}
    }
  return change_count;
}

/*
 * rare and only little gain - but :-)
 lea (-1,a0),a1
 add.l d1,a1
 subq.l #1,d1
 ->
 move.l a0,a1
 subq.l #1,d1
 add.l d1,a1
 */
static unsigned
opt_merge_add (void)
{
  unsigned change_count = 0;
  for (unsigned index = 0; index + 2 < infos.size (); ++index)
    {
      insn_info & ii0 = infos[index];
      insn_info & ii1 = infos[index + 1];
      insn_info & ii2 = infos[index + 2];

      if (!ii2.is_dst_reg ())
	{
	  index += 2;
	  continue;
	}

      if (!ii1.is_dst_reg ())
	{
	  ++index;
	  continue;
	}

      if (!ii0.is_dst_reg () || !ii0.is_src_plus () || !ii1.is_src_plus () || !ii2.is_src_plus ())
	continue;

      if (!ii0.is_src_const () || !ii2.is_src_const () || ii0.get_src_intval () != ii2.get_src_intval ())
	continue;

      if (ii0.get_dst_regno () != ii1.get_dst_regno () || ii1.get_src_regno () != ii2.get_dst_regno ())
	continue;

      rtx_insn * insn1 = ii1.get_insn ();

      CC_STATUS_INIT;
      NOTICE_UPDATE_CC (PATTERN (insn1), insn1);
      if (cc_status.value1 || cc_status.value2)
	continue;

      log ("%d: merge_add applied\n", index);

      rtx_insn * insn0 = ii0.get_insn ();
      rtx set = PATTERN (insn0);

      // convert lea (-1,a0),a1 into move.l a0,a1
      rtx_insn * newins0 = make_insn_raw (gen_rtx_SET(XEXP(set, 0), XEXP(XEXP(set, 1), 0)));
      add_insn_after (newins0, insn0, 0);
      SET_INSN_DELETED(insn0);
      // update infos accordingly
      ii0.plus_to_move (newins0);

      rtx_insn * insn2 = ii2.get_insn ();
      rtx_insn * newins1 = make_insn_raw (PATTERN (insn1));
      add_insn_after (newins1, insn2, 0);
      SET_INSN_DELETED(insn1);
      ii1.swap_adds (newins1, ii2);

      ++change_count;
    }
  return change_count;
}

/**
 * 1. scan for all used registers.
 * 2. scan the stack from for omittable push/pop
 * 3. adjust stack frame + insns referring to stack pointer
 * typical code:
 subq.l #4,sp
 movem.l #16190,-(sp)
 move.l 52(sp),d2
 move.l 56(sp),d3

 * or
 link a5,#4
 movem.l #16190,-(sp)
 move.l 8(a5),d2
 move.l 12(a5),d3
 *
 * => with a5 check only prolog/epilog
 * => without a5 adjust insns referring sp if offset > startoffset + current sp diff
 *
 * startvalue count(pushes)*4
 * newstartvalue = startvalue - omitted pushes
 */
static unsigned
opt_shrink_stack_frame (void)
{
  /* nothing to do. */
  if (!infos.size ())
    return 0;

  std::vector<int> a5pos;

  unsigned pos = 0;
  rtx_insn * insn = infos[pos].get_insn ();
  if (JUMP_P(insn)) /* return -> empty function*/
    return 0;

  bool usea5 = false;
  int paramstart = 4;
  int a5offset = 0;

  /*
   * Move prologue to temp.
   * Only register push and parallel insn unless its a link a5 are moved.
   */
  for (; pos < infos.size ();)
    {
      insn_info & ii = infos[pos];
      insn = ii.get_insn ();

      if (ii.in_proepi () != 1)
	break;

      rtx pattern = PATTERN (insn);
      if (GET_CODE(pattern) == PARALLEL)
	{
	  rtx set = XVECEXP(pattern, 0, 0);
	  rtx dst = SET_DEST(set);
	  ii.mark_stack ();
	  /* ignore link a5 */
	  if (REG_P(dst) && REGNO(dst) == FRAME_POINTER_REGNUM)
	    {
	      a5pos.push_back (pos);
	      usea5 = true;
	      set = XVECEXP(pattern, 0, 2);
	      a5offset = INTVAL(XEXP(SET_SRC(set), 1));
	    }
	  ++pos;
	  continue;
	}
      if (GET_CODE(pattern) != SET)
	{
	  /* (set (mem:BLK (scratch) [0  A8]) (unspec:BLK [ ...)) */
	  if (MEM_P(SET_DEST(pattern)) && GET_CODE(SET_SRC(pattern)) == UNSPEC)
	    a5pos.push_back (pos);
	  ++pos;
	  continue;
	}

      /* move only the push statements. */
      rtx src = SET_SRC(pattern);
      rtx dest = SET_DEST(pattern);
      if (REG_P(src))
	{
	  if (MEM_P(dest))
	    {
	      rtx predec = XEXP(dest, 0);
	      if (GET_CODE(predec) == PRE_DEC)
		{
		  rtx reg = XEXP(predec, 0);
		  if (REG_P(reg) && REGNO(reg) == STACK_POINTER_REGNUM)
		    {
		      ii.mark_stack ();
		    }
		}
	    }
	}
      else if (GET_CODE(src) == PLUS && REG_P(dest) && REGNO(dest) == STACK_POINTER_REGNUM)
	{
	  /* check for stack variables. */
	  rtx reg = XEXP(src, 0);
	  rtx cx = XEXP(src, 1);
	  if (REG_P(reg) && REGNO(reg) == STACK_POINTER_REGNUM && CONST_INT_P(cx))
	    paramstart -= INTVAL(cx);
	}

      if (++pos >= infos.size ())
	{
	  return 0;
	}
    }

  if (pos == 0)
    return 0;

  unsigned prologueend = pos;

  /* search epilogues - there can be multiple epilogues. */
  while (pos < infos.size ())
    {
      while (pos < infos.size ())
	{
	  if (infos[pos].in_proepi ())
	    break;
	  ++pos;
	}

      /* move epilogues away. */
      for (; pos < infos.size (); ++pos)
	{
	  insn_info & ii = infos[pos];
	  insn = ii.get_insn ();
	  if (JUMP_P(insn) || LABEL_P(insn) || !ii.in_proepi ())
	    break;

	  /* omit the frame pointer a5. */
	  rtx pattern = PATTERN (insn);
	  if (GET_CODE(pattern) == PARALLEL)
	    {
	      rtx set = XVECEXP(pattern, 0, 0);
	      rtx dst = SET_DEST(set);
	      ii.mark_stack ();
	      /* unlink is last. */
	      if (REG_P(dst) && REGNO(dst) == FRAME_POINTER_REGNUM)
		{
		  a5pos.push_back (pos);
		  break;
		}

	    }
	  else if (GET_CODE(pattern) == SET)
	    {
	      /* check for move (a7+), x */
	      rtx src = SET_SRC(pattern);
	      rtx dst = SET_DEST(pattern);
	      if (REG_P(dst))
		{
		  if (MEM_P(src))
		    {
		      rtx postinc = XEXP(src, 0);
		      if (GET_CODE(postinc) == POST_INC)
			{
			  rtx reg = XEXP(postinc, 0);
			  if (REG_P(reg) && REGNO(reg) == STACK_POINTER_REGNUM)
			    ii.mark_stack ();
			}
		      else if (GET_CODE(postinc) == PLUS)
			{
			  rtx a5 = XEXP(postinc, 0);
			  if (REG_P(a5) && REGNO(a5) == FRAME_POINTER_REGNUM)
			    ii.mark_stack ();
			}
		    }
		}
	    }
	}
      ++pos;
    }
  /* gather usage stats without prologue/epilogue */
  insn_info ii;
  for (unsigned i = 0; i < infos.size (); ++i)
    {
      insn_info & jj = infos[i];
      if (jj.in_proepi ())
	continue;

      ii.or_use (jj);
    }
  unsigned freemask = ~ii.get_use () & 0x7fff;

  rtx a7 = gen_raw_REG (SImode, STACK_POINTER_REGNUM);
  rtx a5 = gen_raw_REG (SImode, FRAME_POINTER_REGNUM);

  unsigned changed = 0;
  unsigned adjust = 0;
  /* now all push/pop insns are in temp. */
  for (unsigned i = 0; i < infos.size (); ++i)
    {
      insn_info & ii = infos[i];
      if (!ii.is_stack ())
	continue;

      insn = ii.get_insn ();
      rtx pattern = PATTERN (insn);
      /* check the pushed regs, either a vector or single statements */
      if (GET_CODE(pattern) == PARALLEL)
	{
	  std::vector<rtx> regs;
	  std::vector<rtx> clobbers;
	  for (int j = 0; j < XVECLEN(pattern, 0); ++j)
	    {
	      rtx set = XVECEXP(pattern, 0, j);
	      if (GET_CODE(set) == CLOBBER)
		{
		  clobbers.push_back (set);
		  continue;
		}
	      rtx dst = SET_DEST(set);
	      rtx src = SET_SRC(set);
	      rtx reg;
	      if (MEM_P(src))
		reg = dst;
	      else if (MEM_P(dst))
		reg = src;
	      else
		continue;

	      if (REGNO(reg) == FRAME_POINTER_REGNUM)
		{
		  // mark as "do not touch"
		  clobbers.push_back (reg);
		  break;
		}

	      if (i < prologueend)
		paramstart += 4;
	      unsigned regbit = 1 << REGNO(reg);
	      if (freemask & regbit)
		{
		  log (i < prologueend ? "remove push for %s\n" : "remove pop for %s\n",
		  reg_names[REGNO(reg)]);
		  if (i < prologueend)
		    adjust += 4;
		}
	      else
		regs.push_back (copy_reg (reg, -1));
	    }

	  /* don't touch - clobbers! */
	  if (clobbers.size ())
	    continue;

	  /* add romm for add.
	   * push is always using -(a7) addressing.
	   * If a5 is used a movem offset(a5) is generated to pop saved registers..
	   * Otherwise a7 is used and with (a7)+ addressing.
	   */
	  int add1 = i < prologueend || !usea5 ? 1 : 0;
	  if ((int) regs.size () + add1 < XVECLEN(pattern, 0) || regs.size () <= 2)
	    {
	      log ("shrinking stack frame from %d to %d\n", XVECLEN(pattern, 0) - add1, regs.size ());
	      if (regs.size () <= 2)
		{
		  changed = 1;
		  for (unsigned k = 0; k < regs.size (); ++k)
		    {
		      rtx reg = regs[k];
		      if (i < prologueend)
			{
			  /* push */
			  rtx dec = gen_rtx_PRE_DEC(REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, a7);
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, dec);
			  rtx set = gen_rtx_SET(mem, reg);
			  emit_insn_after (set, insn);
			}
		      else
			{
			  /* pop */
			  rtx dec = gen_rtx_POST_INC(REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, a7);
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, dec);
			  rtx set = gen_rtx_SET(reg, mem);
			  emit_insn_before (set, insn);
			}
		    }
		}
	      else
		{
		  rtx parallel = gen_rtx_PARALLEL(VOIDmode, rtvec_alloc (regs.size () + add1));
		  rtx plus;

		  int x = 0;
		  for (unsigned k = 0; k < regs.size (); ++k)
		    x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;

		  /* no add if a5 is used with pop */
		  if (!usea5 || i < prologueend)
		    {
		      plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, i < prologueend ? -x : x));
		      XVECEXP(parallel, 0, 0) = gen_rtx_SET(a7, plus);
		    }

		  if (i >= prologueend)
		    x = usea5 ? -x : 0;

		  for (unsigned k = 0; k < regs.size (); ++k)
		    {
		      if (i < prologueend)
			{
			  /* push */
			  plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, -x));
			  x -= REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			  rtx set = gen_rtx_SET(mem, regs[k]);
			  XVECEXP(parallel, 0, k + 1) = set;
			}
		      else
			{
			  /* pop */
			  if (usea5)
			    {
			      x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			      plus = gen_rtx_PLUS(SImode, a5, gen_rtx_CONST_INT (SImode, a5offset + x));
			      rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			      rtx set = gen_rtx_SET(regs[k], mem);
			      XVECEXP(parallel, 0, k) = set;
			    }
			  else
			    {
			      plus = x ? gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, x)) : a7;
			      x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			      rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			      rtx set = gen_rtx_SET(regs[k], mem);
			      XVECEXP(parallel, 0, k + 1) = set;
			    }
			}
		    }
		  emit_insn_after (parallel, insn);
		}
	      SET_INSN_DELETED(insn);
	      changed = 1;
	    }
	}
      else
	{
	  rtx set = PATTERN (insn);

	  if (i < prologueend)
	    {
	      /* move x,-(a7). */
	      rtx src = SET_SRC(set);
	      paramstart += REGNO(src) > STACK_POINTER_REGNUM ? 12 : 4;
	      unsigned regbit = 1 << REGNO(src);
	      if (freemask & regbit)
		{
		  adjust += REGNO(src) > STACK_POINTER_REGNUM ? 12 : 4;
		  log ("remove push for %s\n", reg_names[REGNO(src)]);
		  SET_INSN_DELETED(insn);
		  ++changed;
		}
	    }
	  else
	    {
	      /* move (a7)+,x */
	      rtx dst = SET_DEST(set);
	      unsigned regbit = 1 << REGNO(dst);
	      if (freemask & regbit)
		{
		  log ("remove pop for %s\n", reg_names[REGNO(dst)]);
		  SET_INSN_DELETED(insn);
		  ++changed;
		}
	    }
	}
    }

  /* fix sp offsets. */
  if (!usea5 && adjust)
    {
      for (unsigned index = 0; index < infos.size (); ++index)
	{
	  insn_info & ii = infos[index];
	  insn = ii.get_insn ();
	  if (!insn || !INSN_P(insn))
	    continue;

	  rtx set = single_set (insn);
	  if (!set)
	    continue;

	  rtx mem = SET_SRC(set);
	  if (MEM_P(mem))
	    {
	      rtx plus = XEXP(mem, 0);
	      if (GET_CODE(plus) == PLUS)
		{
		  rtx sp = XEXP(plus, 0);
		  if (REG_P(sp) && REGNO(sp) == STACK_POINTER_REGNUM)
		    {
		      rtx c = XEXP(plus, 1);
		      if (CONST_INT_P(c))
			{
			  int n = INTVAL(c);
			  if (n >= paramstart)
			    XEXP(plus, 1) = gen_rtx_CONST_INT (SImode, n - adjust);
			}
		    }
		}
	    }
	}
    }

  if (usea5 && a5offset == -4)
    {
      if (freemask & (1 << FRAME_POINTER_REGNUM))
	{
	  log ("dropping unused frame pointer\n");
	  for (std::vector<int>::iterator i = a5pos.begin (); i != a5pos.end (); ++i)
	    SET_INSN_DELETED(infos[*i].get_insn ());

	  /* convert parameter access via a5 into a7. */
	  for (unsigned i = 0; i < infos.size (); ++i)
	    {
	      insn_info & ii = infos[i];

	      if (ii.is_dst_reg () && ii.get_src_mem_regno () == FRAME_POINTER_REGNUM)
		{
		  rtx x = gen_rtx_CONST_INT (SImode, ii.get_src_intval () - 4);
		  rtx p = gen_rtx_PLUS(SImode, a7, x);
		  rtx pattern = gen_rtx_SET(copy_reg (ii.get_dst_reg (), -1),
					    gen_rtx_MEM (GET_MODE(ii.get_dst_reg ()), p));
		  set_insn_deleted (ii.get_insn ());
		  rtx_insn * newinsn = emit_insn_after (pattern, ii.get_insn ());
		  ii.plus_to_move (newinsn);
		}
	    }

	  ++changed;
	}
    }

  return changed;
}

/*
 * Convert a series of move into absolute address into register based moves.
 */
static unsigned
opt_absolute (void)
{
  unsigned change_count = 0;

  for (unsigned i = 0; i < infos.size (); ++i)
    {
      insn_info & ii = infos[i];
      if (!ii.is_dst_mem () || !ii.has_dst_addr () || ii.has_dst_memreg ())
	continue;

      unsigned freemask = ~(ii.get_use () | ii.get_def ()) & 0x7f00;
      if (!freemask)
	continue;

      std::vector<unsigned> found;
      found.push_back (i);
      unsigned base = ii.get_dst_addr ();
      unsigned j = i + 1;
      for (; j < infos.size (); ++j)
	{
	  insn_info & jj = infos[j];
	  if (jj.is_label () || jj.is_jump () || jj.is_call ())
	    break;

	  freemask &= ~(jj.get_use () | jj.get_def ());
	  if (!freemask)
	    break;

	  if (jj.is_dst_mem () && jj.has_dst_addr () && !jj.has_dst_memreg ())
	    {
	      unsigned addr = jj.get_dst_addr ();
	      if (addr < base)
		base = addr;
	      if (addr - base > 0x7ffc)
		continue;

	      found.push_back (j);
	    }
	}

      if (freemask && found.size () > 2)
	{
	  unsigned regno = bit2regno (freemask);
	  log ("modifying %d immediate using %s\n", found.size (), reg_names[regno]);

	  for (auto k = found.begin (); k != found.end (); ++k)
	    {
	      insn_info & kk = infos[*k];
	      kk.absolute2base (regno, base);
	    }

	  // load base into reg
	  rtx lea = gen_rtx_SET(gen_raw_REG (SImode, regno), gen_rtx_CONST_INT (SImode, base));
	  rtx_insn * insn = emit_insn_before (lea, ii.get_insn ());
	  insn_info nn (insn);
	  nn.set_use (ii.get_use ());
	  nn.scan ();
	  nn.fledder (lea);
	  nn.mark_def (regno);
	  infos.insert (infos.begin () + i, nn);
	  while (i++ < j)
	    infos[i].mark_use (regno);
	  ++j;
	  ++change_count;
	}

      i = j;
    }

  if (change_count)
    update_insn2index ();

  return change_count;
}

namespace
{

  const pass_data pass_data_bbb_optimizations =
    { RTL_PASS, /* type */
    "bebbo's-optimizers", /* name */
    OPTGROUP_NONE, /* optinfo_flags */
    TV_NONE, /* tv_id */
    0, /* properties_required */
    0, /* properties_provided */
    0, /* properties_destroyed */
    0, /* todo_flags_start */
    0, //( TODO_df_finish | TODO_df_verify), /* todo_flags_finish */
      };

  class pass_bbb_optimizations : public rtl_opt_pass
  {
  public:
    pass_bbb_optimizations (gcc::context *ctxt) :
	rtl_opt_pass (pass_data_bbb_optimizations, ctxt), pp (0)
    {
    }

    /* opt_pass methods: */
    virtual bool
    gate (function *)
    {
      if (!string_bbb_opts)
	string_bbb_opts = "+";

      return TARGET_AMIGA && optimize > 0 && string_bbb_opts && !strchr (string_bbb_opts, '-');
    }

    virtual unsigned int
    execute (function *)
    {
      return execute_bbb_optimizations ();
    }

    opt_pass *
    clone ()
    {
      pass_bbb_optimizations * bbb = new pass_bbb_optimizations (m_ctxt);
      bbb->pp = pp + 1;
      return bbb;
    }

    unsigned int pp;

    unsigned
    execute_bbb_optimizations (void);
  };
// class pass_bbb_optimizations

  /* Main entry point to the pass.  */
  unsigned
  pass_bbb_optimizations::execute_bbb_optimizations (void)
  {
    be_very_verbose = strchr (string_bbb_opts, 'V');
    be_verbose = be_very_verbose || strchr (string_bbb_opts, 'v') || 1;

    bool do_opt_strcpy = strchr (string_bbb_opts, 's') || strchr (string_bbb_opts, '+');
    bool do_commute_add_move = strchr (string_bbb_opts, 'a') || strchr (string_bbb_opts, '+');
    bool do_propagate_moves = strchr (string_bbb_opts, 'p') || strchr (string_bbb_opts, '+');
    bool do_const_cmp_to_sub = strchr (string_bbb_opts, 'c') || strchr (string_bbb_opts, '+');
    bool do_merge_add = strchr (string_bbb_opts, 'm') || strchr (string_bbb_opts, '+');
    bool do_elim_dead_assign = strchr (string_bbb_opts, 'e') || strchr (string_bbb_opts, '+');
    bool do_bb_reg_rename = strchr (string_bbb_opts, 'r') || strchr (string_bbb_opts, '+');
    bool do_shrink_stack_frame = strchr (string_bbb_opts, 'f') || strchr (string_bbb_opts, '+');
    bool do_absolute = strchr (string_bbb_opts, 'b') || strchr (string_bbb_opts, '+');

    for (;;)
      {
	int done = 1;
	update_insns ();
	if (do_opt_strcpy && opt_strcpy ())
	  done = 0, update_insns ();

	if (do_commute_add_move && opt_commute_add_move ())
	  done = 0, update_insns ();

	if (do_propagate_moves && opt_propagate_moves ())
	  done = 0, update_insns ();

	if (do_const_cmp_to_sub && opt_const_cmp_to_sub ())
	  done = 0, update_insns ();

	if (do_merge_add && opt_merge_add ())
	  done = 0;

	if (do_elim_dead_assign && opt_elim_dead_assign ())
	  done = 0, update_insns ();

	if (do_absolute && opt_absolute ())
	  done = 0;

	if (do_bb_reg_rename)
	  {
	    while (opt_reg_rename ())
	      {
		update_insns ();
		done = 0;
	      }
	  }

	if (done)
	  break;
      }

    if (do_shrink_stack_frame)
      {
	if (opt_shrink_stack_frame ())
	  update_insns ();
      }

    if (strchr (string_bbb_opts, 'X') || strchr (string_bbb_opts, 'x'))
      dump_insns ("bbb", strchr (string_bbb_opts, 'X'));

    return 0;
  }

}      // anon namespace

rtl_opt_pass *
make_pass_bbb_optimizations (gcc::context * ctxt)
{
  return new pass_bbb_optimizations (ctxt);
}
