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

extern struct lang_hooks lang_hooks;

/* Lookup of the current function name. */
extern tree current_function_decl;
static tree last_function_decl;
static char fxname[512];
static char const *
get_current_function_name ()
{
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
  rtx_code src_op;
  bool src_ee;
  bool src_const;

  machine_mode mode;

  rtx dst_reg;
  rtx dst_mem_reg;
  rtx dst_symbol;
  rtx src_reg;
  rtx src_mem_reg;
  rtx src_symbol;
  unsigned dst_mem_addr;
  int src_intval;
  unsigned src_mem_addr;

public:
  insn_info (rtx_insn * i = 0, int p = 0) :
      insn (i), myuse (0), hard (0), use (0), def (0), proepi (p), stack (false), label (false), jump (false), call (
	  false), compare (false), dst_mem (false), src_mem (false), dst_plus (false), src_plus (false), src_op (
	  (rtx_code) 0), src_ee (false), src_const (false), mode (VOIDmode), dst_reg (0), dst_mem_reg (0), dst_symbol (
	  0), src_reg (0), src_mem_reg (0), src_symbol (0), dst_mem_addr (0), src_intval (0), src_mem_addr (0)
  {
  }

  int
  get_index () const;

  void
  plus_to_move (rtx_insn * newinsn);

  void
  swap_adds (rtx_insn * newinsn, insn_info & ii);

  void
  absolute2base (unsigned regno, unsigned base, rtx with_symbol);

  inline bool
  is_compare () const
  {
    return compare;
  }

  inline machine_mode
  get_mode () const
  {
    return mode;
  }

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
  has_src_memreg () const
  {
    return src_mem_reg;
  }

  inline rtx
  get_dst_symbol () const
  {
    return dst_symbol;
  }

  inline rtx
  get_src_symbol () const
  {
    return src_symbol;
  }
  inline bool
  has_dst_addr () const
  {
    return dst_mem_addr;
  }

  inline bool
  has_src_addr () const
  {
    return src_mem_addr;
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

  inline unsigned
  get_src_mem_addr () const
  {
    return src_mem_addr;
  }

  inline bool
  is_src_reg () const
  {
    return src_reg && !src_op;
  }

  inline int
  get_src_op () const
  {
    return src_op;
  }

  inline bool
  is_src_ee () const
  {
    return src_ee;
  }

  inline bool
  is_src_mem_plus () const
  {
    return src_mem && src_plus;
  }

  inline bool
  is_dst_mem_plus () const
  {
    return dst_mem && dst_plus;
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
  get_dst_mem_regno () const
  {
    return dst_mem_reg ? REGNO(dst_mem_reg) : -1;
  }

  inline int
  get_src_intval () const
  {
    return src_intval;
  }

  inline int
  get_dst_intval () const
  {
    return dst_mem_addr;
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

  inline void
  reset_flags ()
  {
    compare = false;
    dst_mem = false;
    src_mem = false;
    dst_plus = false;
    src_plus = false;
    src_op = (rtx_code) 0;
    src_ee = false;
    src_const = false;

    mode = VOIDmode;

    dst_reg = 0;
    dst_mem_reg = 0;
    dst_symbol = 0;
    src_reg = 0;
    src_mem_reg = 0;
    src_symbol = 0;
    dst_mem_addr = 0;

    src_intval = 0;
    src_mem_addr = 0;
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

  inline unsigned
  get_myuse () const
  {
    return myuse;
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
    myuse = o.myuse;
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
    if (GET_MODE_SIZE(mode) > 4)
      return 0;
    return def & ~hard & ~use & 0x7fff;
  }

  void
  set_insn (rtx_insn * newinsn);

  void
  a5_to_a7 (rtx a7);
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

  if (GET_CODE(dst) == STRICT_LOW_PART || GET_CODE(dst) == SUBREG)
    dst = XEXP(dst, 0);

  mode = GET_MODE(dst);
  if (mode == VOIDmode)
    mode = GET_MODE(src);

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
      else if (GET_CODE(mem) == SYMBOL_REF)
	dst_symbol = mem;
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
      else if (GET_CODE(mem) == CONST)
	{
	  mem = XEXP(mem, 0);
	  if (GET_CODE(mem) == PLUS)
	    {
	      rtx sym = XEXP(mem, 0);
	      if (GET_CODE(sym) == SYMBOL_REF)
		{
		  dst_plus = true;
		  dst_symbol = sym;
		  dst_mem_addr = INTVAL(XEXP(mem, 1));
		}
	    }
	}
    }

  /* It' some kind of operation, e.g. PLUS, XOR, NEG, ... */
  rtx alt_src_reg = 0;
  int code = GET_CODE(src);
  if (!REG_P(src) && !MEM_P(src) && code != CONST_INT && code != CONST && code != CONST_WIDE_INT && code != CONST_DOUBLE
      && code != CONST_FIXED && code != CONST_STRING)
    {
      src_op = GET_CODE(src);
      const char *fmt = GET_RTX_FORMAT(code);
      if (fmt[0] == 'e' && fmt[1] == 'e')
	{
	  src_ee = true;
	  rtx operand = XEXP(src, 1);
	  if (GET_CODE(operand) == CONST_INT || GET_CODE(operand) == CONST_WIDE_INT)
	    src_const = true, src_intval = INTVAL(operand);
	  else if (REG_P(operand))
	    alt_src_reg = operand;
	}
      src = XEXP(src, 0);
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
	src_mem_addr = INTVAL(mem);
      else if (GET_CODE(mem) == SYMBOL_REF)
	src_symbol = mem;
      else if (GET_CODE(mem) == PLUS)
	{
	  src_plus = true;
	  rtx reg = XEXP(mem, 0);
	  rtx konst = XEXP(mem, 1);
	  if (REG_P(reg) && GET_CODE(konst) == CONST_INT)
	    {
	      src_mem_reg = reg;
	      src_const = true;
	      src_mem_addr = INTVAL(konst);
	    }
	}
      else if (GET_CODE(mem) == CONST)
	{
	  mem = XEXP(mem, 0);
	  if (GET_CODE(mem) == PLUS)
	    {
	      rtx sym = XEXP(mem, 0);
	      if (GET_CODE(sym) == SYMBOL_REF)
		{
		  src_plus = true;
		  src_symbol = sym;
		  src_mem_addr = INTVAL(XEXP(mem, 1));
		}
	    }
	}
    }
  else if (GET_CODE(src) == CONST_INT)
    {
      src_const = true;
      src_intval = INTVAL(src);
    }
  if (alt_src_reg)
    src_reg = alt_src_reg;
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
typedef std::vector<insn_info>::iterator insn_info_iterator;

static std::multimap<int, rtx_insn *> label2jump;
typedef std::multimap<int, rtx_insn *>::iterator l2j_iterator;

static std::map<rtx_insn *, insn_info *> insn2index;
typedef std::map<rtx_insn *, insn_info *>::iterator i2i_iterator;

static insn_info * info0;
static unsigned usable_regs;

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
  src_op = (rtx_code) 0;
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

static
void
replace_reg (rtx x, unsigned regno, rtx newreg, int offset)
{
  RTX_CODE code = GET_CODE(x);
  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx y = XEXP(x, i);
	  if (REG_P(y) && REGNO(y) == regno)
	    {
	      XEXP(x, i) = newreg;
	      if (offset && i + 1 < GET_RTX_LENGTH(code))
		{
		  rtx c = XEXP(x, i + 1);
		  if (GET_CODE(c) == CONST_INT)
		    XEXP(x, i + 1) = gen_rtx_CONST_INT (GET_MODE(x), INTVAL(c) + offset);
		}
	    }
	  else
	    replace_reg (y, regno, newreg, offset);
	}
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  replace_reg (XVECEXP(x, i, j), regno, newreg, offset);
    }
}

void
insn_info::a5_to_a7 (rtx a7)
{
  replace_reg (PATTERN (insn), FRAME_POINTER_REGNUM, a7, -4);
}

void
insn_info::set_insn (rtx_insn * newinsn)
{
  insn = newinsn;

  reset_flags ();

  fledder (PATTERN (insn));
}

void
insn_info::absolute2base (unsigned regno, unsigned base, rtx with_symbol)
{
  rtx set = PATTERN (get_insn ());
  rtx src = SET_SRC(set);
  rtx dst = SET_DEST(set);

  rtx pattern;
  rtx reg = gen_raw_REG (SImode, regno);

  if (is_dst_mem () && (has_dst_addr () || get_dst_symbol ()) && !has_dst_memreg () && get_dst_symbol () == with_symbol)
    {
      unsigned addr = get_dst_addr ();
      unsigned offset = addr - base;
      if (offset <= 0x7ffe)
	{
	  rtx olddst = dst;
	  if (base == addr)
	    dst = gen_rtx_MEM (mode, reg);
	  else
	    dst = gen_rtx_MEM (mode, gen_rtx_PLUS(SImode, reg, gen_rtx_CONST_INT (SImode, offset)));

	  dst_mem_reg = reg;
	  dst_mem = true;
	  dst_mem_addr = offset;
	  dst_plus = offset != 0;
	}
    }

  if (is_src_mem () && (has_src_addr () || get_src_symbol ()) && !has_src_memreg () && get_src_symbol () == with_symbol)
    {
      unsigned addr = get_src_mem_addr ();
      unsigned offset = addr - base;
      if (offset <= 0x7ffe)
	{
	  if (base == addr)
	    src = gen_rtx_MEM (mode, reg);
	  else
	    src = gen_rtx_MEM (mode, gen_rtx_PLUS(SImode, reg, gen_rtx_CONST_INT (SImode, offset)));

	  /* some operation to the same value as dst. eg. eor #5,symbol+8 -> eor #5,8(ax) */
	  if (src_op)
	    {
	      if (src_ee)
		src = gen_rtx_fmt_ee(src_op, mode, src, gen_rtx_CONST_INT (mode, src_intval));
	      else
		{
		  if (src_op == SIGN_EXTEND)
		    {
		      PUT_MODE_RAW(src, mode == SImode ? HImode : mode == HImode ? QImode : SImode);
		      src->call = 1;
		    }
		  src = gen_rtx_fmt_e(src_op, mode, src);
		}
	    }

	  src_mem_reg = reg;
	  src_mem = true;
	  src_mem_addr = offset;
	  src_plus = offset != 0;
	}
    }

  pattern = gen_rtx_SET(dst, src);

  SET_INSN_DELETED(insn);
  insn = emit_insn_after (pattern, insn);

  mark_use (regno);

  insn2index.insert (std::make_pair (insn, this));
}
/*
 * Reset collected data.
 */
static void
clear (void)
{
  label2jump.clear ();
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

  i2i_iterator i = insn2index.find (insn);
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
	      for (l2j_iterator i = label2jump.find (insn->u2.insn_uid), k = i;
		  i != label2jump.end () && i->first == k->first; ++i)
		{
		  i2i_iterator j = insn2index.find (i->second);
		  if (j != insn2index.end ())
		    todo.push_back (std::make_pair (j->second->get_index (), ii));
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
	      NOTICE_UPDATE_CC(PATTERN (insn), insn);
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

  /* fill the mask of general used regs. */
  insn_info zz;
  for (unsigned i = 0; i < infos.size (); ++i)
    {
      insn_info & ii = infos[i];
      if (ii.in_proepi () != 1)
	break;

      zz.or_use (ii);
    }

  /* always allow a0/a1, d0/d1. */
  usable_regs = zz.get_use () | 0x303;
  if (flag_pic)
    usable_regs &= ~(1 << PIC_REG);

  if (infos.size () && infos[0].is_use (FRAME_POINTER_REGNUM))
    usable_regs &= ~(1 << FRAME_POINTER_REGNUM);

  usable_regs &= ~(1 << STACK_POINTER_REGNUM);
}

enum AbortCodes
{
  E_OK, E_NO_JUMP_LABEL, E_JUMP_TABLE_MISMATCH, E_JUMP_GOTO_LABEL
};

/*
 * Create a filtered view of insns - keep only those to work with.
 */
static unsigned
update_insns ()
{
  rtx_insn *insn, *next;
  unsigned result = 0;
  rtx jump_table = 0;

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
	      inproepilogue = 0;

	      if (ANY_RETURN_P(PATTERN (insn)))
		continue;

	      ii.mark_jump ();
	      if (jump_table)
		{
		  if (XEXP(jump_table, 0) != insn)
		    {
		      if (be_very_verbose)
			{
			  debug_rtx (insn);
			  debug_rtx (jump_table);
			}
		      result = E_JUMP_TABLE_MISMATCH;
		      jump_table = 0;
		      continue;
		    }

		  // -> jump_table_data
		  rtx table = PATTERN (XEXP(jump_table, 1));
		  if (GET_CODE(table) == ADDR_DIFF_VEC || GET_CODE(table) == ADDR_VEC)
		    {
		      int k = GET_CODE(table) == ADDR_DIFF_VEC;
		      for (int j = 0; j < XVECLEN(table, k); ++j)
			{
			  rtx ref = XVECEXP(table, k, j);
			  if (!LABEL_REF_NONLOCAL_P(ref))
			    {
			      rtx label = XEXP(ref, 0);
			      label2jump.insert (std::make_pair (label->u2.insn_uid, insn));
			    }
			}
		    }
		  else
		    {
		      if (be_very_verbose)
			{
			  debug_rtx (insn);
			  debug_rtx (jump_table);
			}
		      result = E_JUMP_GOTO_LABEL;
		      jump_table = 0;
		      continue;
		    }
		  jump_table = 0;
		}
	      else
		{
		  rtx_insn * label = (rtx_insn *) JUMP_LABEL(insn);
		  if (!label)
		    {
		      if (be_very_verbose)
			debug_rtx (insn);
		      result = E_NO_JUMP_LABEL;
		      continue;
		    }
		  label2jump.insert (std::make_pair (label->u2.insn_uid, insn));
		}
	    }
	  else if (LABEL_P(insn))
	    {
	      ii.mark_label ();
	      jump_table = 0;
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

	      for (rtx next, note = REG_NOTES(insn); note; note = next)
		{
		  next = XEXP(note, 1);
		  if (REG_NOTE_KIND (note) == REG_LABEL_OPERAND)
		    {
		      jump_table = XEXP(note, 0);
		    }
		}

	    }
	}
      else if (NOTE_P(insn))
	{
	  if (NOTE_KIND(insn) == NOTE_INSN_PROLOGUE_END)
	    inproepilogue = 0;
	  else if (NOTE_KIND(insn) == NOTE_INSN_EPILOGUE_BEG)
	    inproepilogue = 2;
	}
    }

  update_insn2index ();
  update_insn_infos ();

  return result;
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
		  for (l2j_iterator i = label2jump.find (insn->u2.insn_uid), k = i;
		      i != label2jump.end () && i->first == k->first; ++i)
		    {
		      i2i_iterator j = insn2index.find (i->second);
		      if (j == insn2index.end ())
			continue;

		      unsigned start = j->second->get_index ();
		      if (!infos[start].is_use (rename_regno))
			continue;

		      start = find_start (found, start, rename_regno);
		      todo.push_back (start);
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

	      /* abort if some insn using this reg uses more than 1 reg. */
	      if ((jj.get_myuse () & rename_regbit) && GET_MODE_SIZE(jj.get_mode()) > 4)
		{
		  mask = 0;
		  break;
		}

	      /* update free regs. */
	      mask &= ~jj.get_use ();
	      mask &= ~jj.get_def ();
	      if (!mask)
		break;

	      found.insert (pos);

	      /* follow jump and/or next insn. */
	      if (JUMP_P(insn))
		{
		  i2i_iterator j = insn2index.find ((rtx_insn *) JUMP_LABEL(insn));
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

	  log ("(r) opt_reg_rename %s -> %s (%d locs, start at %d)\n", reg_names[oldregno], reg_names[newregno],
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

			      log ("(p) propagate_moves condition met, moving regs %s, %s\n",
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
				  log ("(p) propagate_moves fixing %d jump outs\n", jump_out.size ());

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
		  NOTICE_UPDATE_CC(PATTERN (reg2x), reg2x);
		  if (cc_status.flags == 0 && rtx_equal_p (dst, cc_status.value2))
		    {
		      int num_clobbers_to_add = 0;
		      int insn_code_number;

		      rtx pattern = gen_rtx_SET(SET_DEST(single_set(reg2x)), SET_SRC(single_set (x2reg)));
		      rtx_insn * newinsn = make_insn_raw (pattern);
		      insn_code_number = recog (PATTERN (newinsn), newinsn, &num_clobbers_to_add);
		      if (insn_code_number >= 0 && check_asm_operands (PATTERN (newinsn)))
			{
			  log ("(s) opt_strcpy condition met, removing compare and joining insns - omit reg %s\n",
			  reg_names[REGNO(dst)]);

			  emit_insn_after (pattern, reg2x);

			  SET_INSN_DELETED(x2reg);
			  SET_INSN_DELETED(reg2x);
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
	  log ("(a) commute_add_move found\n");

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
  if (infos.size () < 2)
    return change_count;

  unsigned lastsub = 0;
  for (unsigned index = infos.size () - 2; index > 0; --index)
    {
      insn_info & i1 = infos[index];

      /* we wan't a compare or tst insn, */
      if (!i1.is_compare ())
	continue;

      if (GET_MODE_SIZE(i1.get_mode()) > 4 || !i1.is_dst_reg () || REGNO(i1.get_dst_reg()) > 7)
	continue;

      /* src must be a reg dead register with a constant - or a #0 */
      if (!i1.get_src_reg () && (!i1.is_src_const () || i1.get_src_op () == PLUS))
	continue;

      /* allow an alive reg, if life ends at previous handled sub. */
      int lastsubval = 0;
      if (lastsub == index + 3)
	{
	  insn_info & pp = infos[lastsub];
	  if (pp.get_dst_regno () != i1.get_dst_regno ())
	    continue;
	  lastsubval = pp.get_src_intval ();
	}
      else if (!is_reg_dead (i1.get_dst_regno (), index))
	continue;

      insn_info & i0 = infos[index - 1];
      int intval = 0;
      /* compare with register - check previous insn for load with constant. */
      if (i1.is_src_reg ())
	{
	  if (!is_reg_dead (i1.get_src_regno (), index))
	    continue;

	  if (GET_MODE_SIZE(i0.get_mode()) > 4)
	    continue;

	  if (!i0.is_dst_reg () && (!i0.is_src_const () || i0.get_src_op () == PLUS))
	    continue;

	  if (i0.get_dst_regno () != i1.get_src_regno ())
	    continue;

	  intval = -i0.get_src_intval ();
	  if (intval < -8 || intval > 7)
	    continue;

	  /* is the next sub value in range? */
	  if (lastsub == index + 3 && (lastsubval - intval < -8 || lastsubval - intval > 7))
	    continue;
	}

      /* next insn must be the jump. */
      insn_info & i2 = infos[index + 1];
      if (!i2.is_jump ())
	continue;

      rtx_insn * jump = i2.get_insn ();
      rtx jmppattern = PATTERN (jump);
      if (GET_RTX_LENGTH (GET_CODE(jmppattern)) < 2)
	continue;

      rtx jmpsrc = XEXP(jmppattern, 1);
      if (GET_CODE(jmpsrc) != IF_THEN_ELSE)
	continue;

      rtx condition = XEXP(jmpsrc, 0);
      RTX_CODE code = GET_CODE(condition);
      if (code != EQ && code != NE)
	continue;

      if (intval)
	{
	  rtx copyreg = copy_reg (i1.get_dst_reg (), -1);
	  /* create the sub statement. */
	  rtx sub = gen_rtx_PLUS(i1.get_mode (), copyreg, gen_rtx_CONST_INT (i1.get_mode (), intval));

	  rtx_insn * subinsn = make_insn_raw (gen_rtx_SET(copyreg, sub));

	  int num_clobbers_to_add = 0;
	  int insn_code_number = recog (PATTERN (subinsn), subinsn, &num_clobbers_to_add);
	  if (insn_code_number < 0 || !check_asm_operands (PATTERN (subinsn)))
	    continue;

	  /* delete move #x,dy. */
	  SET_INSN_DELETED(i0.get_insn ())
	  /* delete cmp dx,dy */
	  SET_INSN_DELETED(i1.get_insn ());
	  /* add a cmp #0 - to be removed in final() */

	  /* convert cmp/tst into sub */
	  subinsn = emit_insn_before (PATTERN (subinsn), i1.get_insn ());
	  i1.set_insn (subinsn);

	  rtx neu = gen_rtx_SET(cc0_rtx,
				gen_rtx_COMPARE(i1.get_mode (), copyreg, gen_rtx_CONST_INT(i1.get_mode (), 0)));

	  emit_insn_before (neu, i2.get_insn ());

	  log ("(c) const_cmp_to_sub replaced %s == %s (%d) with sub %d,%s\n", reg_names[i1.get_dst_regno ()],
	  reg_names[i0.get_dst_regno ()],
	       -intval, -intval, reg_names[i1.get_dst_regno ()]);

	  if (index + 3 == lastsub)
	    {
	      /* patch previous sub - or even a compare. */
	      insn_info & pp = infos[lastsub];

	      int diff = lastsubval - intval;
	      rtx c = gen_rtx_CONST_INT (i1.get_mode (), diff);

	      if (pp.is_compare ())
		{
		  /* still a compare with 0 -> insert the sub. */
		  rtx copyreg = copy_reg (i1.get_dst_reg (), -1);
		  /* create the sub statement. */
		  rtx sub = gen_rtx_PLUS(i1.get_mode (), copyreg, c);
		  rtx set = gen_rtx_SET(copyreg, sub);
		  emit_insn_before (set, pp.get_insn ());
		}
	      else
		{
		  /* modify the sub. */
		  XEXP(SET_SRC(PATTERN(pp.get_insn())), 1) = c;
		}
	    }

	  lastsub = index;
	  ++change_count;
	}
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
	  log ("(e) %d: elim_dead_assign to %s\n", index, reg_names[REGNO(dst)]);
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

      if (!ii0.is_dst_reg () || ii0.get_src_op () != PLUS || ii1.get_src_op () != PLUS || ii2.get_src_op () != PLUS)
	continue;

      if (!ii0.is_src_const () || !ii2.is_src_const () || ii0.get_src_intval () != ii2.get_src_intval ())
	continue;

      if (ii0.get_dst_regno () != ii1.get_dst_regno () || ii1.get_src_regno () != ii2.get_dst_regno ())
	continue;

      rtx_insn * insn1 = ii1.get_insn ();

      CC_STATUS_INIT;
      NOTICE_UPDATE_CC(PATTERN (insn1), insn1);
      if (cc_status.value1 || cc_status.value2)
	continue;

      log ("(m) %d: merge_add applied\n", index);

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
	  // do not touch the frame pointer parallel insn.
	  rtx set = XVECEXP(pattern, 0, 0);
	  rtx dst = SET_DEST(set);
	  if (REG_P(dst) && REGNO(dst) == FRAME_POINTER_REGNUM)
	    continue;

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

	      if (i < prologueend)
		paramstart += 4;
	      unsigned regbit = 1 << REGNO(reg);
	      if (freemask & regbit)
		{
		  log (i < prologueend ? "(f) remove push for %s\n" : "(f) remove pop for %s\n",
		  reg_names[REGNO(reg)]);
		  if (i < prologueend)
		    adjust += 4;
		}
	      else
		regs.push_back (copy_reg (reg, -1));
	    }

	  /* add room for add.
	   * push is always using -(a7) addressing.
	   * If a5 is used a movem offset(a5) is generated to pop saved registers..
	   * Otherwise a7 is used and with (a7)+ addressing.
	   */
	  int add1 = i < prologueend || !usea5 ? 1 : 0;
	  if ((int) regs.size () + add1 + (int) clobbers.size () < XVECLEN(pattern, 0) || regs.size () <= 2)
	    {
	      log ("(f) shrinking stack frame from %d to %d\n", XVECLEN(pattern, 0) - add1 - clobbers.size (),
		   regs.size ());
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

		  unsigned l = 0;
		  /* no add if a5 is used with pop */
		  if (!usea5 || i < prologueend)
		    {
		      plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, i < prologueend ? -x : x));
		      XVECEXP(parallel, 0, l) = gen_rtx_SET(a7, plus);
		      ++l;
		    }

		  if (i >= prologueend)
		    x = usea5 ? -x : 0;

		  for (unsigned k = 0; k < regs.size (); ++k, ++l)
		    {
		      if (i < prologueend)
			{
			  /* push */
			  plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, -x));
			  x -= REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			  rtx set = gen_rtx_SET(mem, regs[k]);
			  XVECEXP(parallel, 0, l) = set;
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
			      XVECEXP(parallel, 0, l) = set;
			    }
			  else
			    {
			      plus = x ? gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, x)) : a7;
			      x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			      rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			      rtx set = gen_rtx_SET(regs[k], mem);
			      XVECEXP(parallel, 0, l) = set;
			    }
			}
		    }

		  for (unsigned k = 0; k < clobbers.size (); ++k)
		    {
		      rtx clobber = clobbers[k];
		      XVECEXP(parallel, 0, l++) = clobber;
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
		  log ("(f) remove push for %s\n", reg_names[REGNO(src)]);
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
		  log ("(f) remove pop for %s\n", reg_names[REGNO(dst)]);
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
	  rtx pattern = PATTERN (ii.get_insn ());
	  if (ii.is_compare ())
	    pattern = XEXP(pattern, 1);
	  if (ii.is_src_mem () && ii.is_src_mem_plus () && ii.get_src_mem_regno () == STACK_POINTER_REGNUM)
	    {
	      rtx src = XEXP(pattern, 1);
	      rtx plus = XEXP(src, 0);
	      if (ii.get_src_op ())
		plus = XEXP(plus, 0);
	      XEXP(plus, 1) = gen_rtx_CONST_INT (SImode, ii.get_src_mem_addr () - adjust);
	    }

	  if (ii.is_dst_mem () && ii.is_dst_mem_plus () && ii.get_dst_mem_regno () == STACK_POINTER_REGNUM)
	    {
	      rtx plus = XEXP(XEXP(pattern, 0), 0);
	      XEXP(plus, 1) = gen_rtx_CONST_INT (SImode, ii.get_dst_intval () - adjust);
	    }
	}
    }

  if (usea5 && a5offset == -4)
    {
      /* for now only drop the frame pointer if it's not used.
       * Needs tracking of the sp to adjust the offsets.
       */
      if (freemask & (1 << FRAME_POINTER_REGNUM))
	{
	  log ("(f) dropping unused frame pointer\n");
	  for (std::vector<int>::reverse_iterator i = a5pos.rbegin (); i != a5pos.rend (); ++i)
	    {
	      SET_INSN_DELETED(infos[*i].get_insn ());
	      infos.erase (infos.begin () + *i);
	    }

	  /* convert all parameter accesses via a5 into a7. */
	  for (unsigned i = 0; i < infos.size (); ++i)
	    {
	      insn_info & ii = infos[i];
	      if (ii.get_myuse () & (1 << FRAME_POINTER_REGNUM))
		ii.a5_to_a7 (a7);

	      ii.unset (FRAME_POINTER_REGNUM);
	    }

	  update_insn2index ();
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

      if (ii.is_compare ())
	continue;

      if (ii.get_src_op () && ii.is_src_ee () && !ii.get_src_intval ())
	continue;

      bool is_dst = ii.is_dst_mem () && (ii.has_dst_addr () || ii.get_dst_symbol ()) && !ii.has_dst_memreg ();
      bool is_src = ii.is_src_mem () && (ii.has_src_addr () || ii.get_src_symbol ()) && !ii.has_src_memreg ();

      if (!is_dst && !is_src)
	continue;

      if (ii.get_mode () == VOIDmode)
	continue;

      unsigned freemask = ~(ii.get_use () | ii.get_def ()) & 0x7f00 & usable_regs;
      if (!freemask)
	continue;

      rtx with_symbol = is_dst ? ii.get_dst_symbol () : ii.get_src_symbol ();

      std::vector<unsigned> found;
      found.push_back (i);
      unsigned base = ii.get_dst_addr ();
      unsigned max = base;
      unsigned j = i + 1;
      for (; j < infos.size (); ++j)
	{
	  insn_info & jj = infos[j];
	  /* TODO: continue also at jump target */
	  if (jj.is_jump ())
	    continue;
	  /* TODO: check if label is visited only from jump targets from herein. then the label is ok. */
	  if (jj.is_label ())
	    break;

	  unsigned tempmask = freemask & ~(jj.get_use () | jj.get_def ());
	  if (!tempmask)
	    break;
	  freemask = tempmask;

	  if (jj.get_mode () == VOIDmode || jj.is_compare ())
	    continue;

	  if (jj.get_src_op () && jj.is_src_ee () && !jj.get_src_intval ())
	    continue;

	  bool j_dst = jj.is_dst_mem () && (jj.has_dst_addr () || jj.get_dst_symbol ()) && !jj.has_dst_memreg ()
	      && jj.get_dst_symbol () == with_symbol;
	  bool j_src = jj.is_src_mem () && (jj.has_src_addr () || jj.get_src_symbol ()) && !jj.has_src_memreg ()
	      && jj.get_src_symbol () == with_symbol;

	  /* exclude operations on that symbol. */

	  if (j_dst)
	    {
	      unsigned addr = jj.get_dst_addr ();
	      if (addr < base)
		{
		  if (max - addr <= 0x7ffe)
		    {
		      base = addr;
		      found.push_back (j);
		      continue;
		    }
		}
	      else if (addr - base <= 0x7ffe)
		{
		  if (addr > max)
		    max = addr;
		  found.push_back (j);
		  continue;
		}
	    }
	  if (j_src)
	    {
	      unsigned addr = jj.get_src_mem_addr ();
	      if (addr < base)
		{
		  if (max - addr <= 0x7ffe)
		    {
		      base = addr;
		      found.push_back (j);
		      continue;
		    }
		}
	      else if (addr - base <= 0x7ffe)
		{
		  if (addr > max)
		    max = addr;
		  found.push_back (j);
		  continue;
		}
	    }
	}

      if (freemask && found.size () > 2)
	{
	  /* check again. */
	  for (std::vector<unsigned>::iterator k = found.begin (); k != found.end ();)
	    {
	      insn_info & kk = infos[*k];
	      if (kk.get_dst_addr () - base > 0x7ffc)
		found.erase (k);
	      else
		++k;
	    }
	}
      if (freemask && found.size () > 2)
	{
	  unsigned regno = bit2regno (freemask);
	  if (with_symbol)
	    log ("(b) modifying %d symbol addresses using %s\n", found.size (), reg_names[regno]);
	  else
	    log ("(b) modifying %d absolute addresses using %s\n", found.size (), reg_names[regno]);

	  for (std::vector<unsigned>::iterator k = found.begin (); k != found.end (); ++k)
	    {
	      insn_info & kk = infos[*k];
	      kk.absolute2base (regno, base, with_symbol);
	    }

	  // load base into reg
	  rtx lea;

	  if (with_symbol)
	    {
	      if (base)
		lea = gen_rtx_SET(
		    gen_raw_REG (SImode, regno),
		    gen_rtx_CONST(SImode, gen_rtx_PLUS(SImode, with_symbol, gen_rtx_CONST_INT (SImode, base))));
	      else
		lea = gen_rtx_SET(gen_raw_REG (SImode, regno), with_symbol);
	    }
	  else
	    lea = gen_rtx_SET(gen_raw_REG (SImode, regno), gen_rtx_CONST_INT (SImode, base));
	  rtx_insn * insn = emit_insn_before (lea, ii.get_insn ());
	  insn_info nn (insn);
	  nn.set_use (ii.get_use ());
	  nn.scan ();
	  nn.fledder (lea);
	  nn.mark_def (regno);
	  infos.insert (infos.begin () + i, nn);

	  /* mark until last hit is found. */
	  for (unsigned k = i + 1; k < infos.size (); ++k)
	    {
	      infos[k].mark_use (regno);
	      if (k == *found.rbegin ())
		break;
	    }
	  ++change_count;
	  --i;
	}
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
    be_verbose = be_very_verbose || strchr (string_bbb_opts, 'v');

    bool do_opt_strcpy = strchr (string_bbb_opts, 's') || strchr (string_bbb_opts, '+');
    bool do_commute_add_move = strchr (string_bbb_opts, 'a') || strchr (string_bbb_opts, '+');
    bool do_propagate_moves = strchr (string_bbb_opts, 'p') || strchr (string_bbb_opts, '+');
    bool do_const_cmp_to_sub = strchr (string_bbb_opts, 'c') || strchr (string_bbb_opts, '+');
    bool do_merge_add = strchr (string_bbb_opts, 'm') || strchr (string_bbb_opts, '+');
    bool do_elim_dead_assign = strchr (string_bbb_opts, 'e') || strchr (string_bbb_opts, '+');
    bool do_bb_reg_rename = strchr (string_bbb_opts, 'r') || strchr (string_bbb_opts, '+');
    bool do_shrink_stack_frame = strchr (string_bbb_opts, 'f') || strchr (string_bbb_opts, '+');
    bool do_absolute = strchr (string_bbb_opts, 'b') || strchr (string_bbb_opts, '+');

    if (be_very_verbose)
      log ("ENTER\n");

    unsigned r = update_insns ();
    if (!r)
      {
	for (;;)
	  {
	    int done = 1;
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
      }
    if (r && be_verbose)
      log ("no bbb optimization code %d\n", r);

    if (strchr (string_bbb_opts, 'X') || strchr (string_bbb_opts, 'x'))
      dump_insns ("bbb", strchr (string_bbb_opts, 'X'));

    return r;
  }

}      // anon namespace

rtl_opt_pass *
make_pass_bbb_optimizations (gcc::context * ctxt)
{
  return new pass_bbb_optimizations (ctxt);
}
