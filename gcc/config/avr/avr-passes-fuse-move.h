/* Support for avr-passes.cc for AVR 8-bit microcontrollers.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* FIXME: The documentation in hard-reg-set.h is wrong in that it states
   that HARD_REG_SET is a scalar iff HARD_REG_SET is a macro.
   This is not the case:  HARD_REG_SET is a typedef no matter what.
   So in order to get the lower 32 bits (and maybe more) as a scalar
   we have to invoke type traits as we can't #ifdef HARD_REG_SET  */
template<typename T, typename ELT, bool = std::is_same<T, ELT>::value>
struct elt0_getter;

// All hard regs fit in one HARD_REG_ELT_TYPE: T === ELT.
template<typename T, typename ELT>
struct elt0_getter<T, ELT, true>
{
  static inline const ELT &get (const T &t)
  {
    return t;
  }
};

// HARD_REG_SET is not a scalar but a composite with HARD_REG_ELT_TYPE elts[].
template<typename T, typename ELT>
struct elt0_getter<T, ELT, false>
{
  static inline const ELT &get (const T &t)
  {
    return t.elts[0];
  }
};


// To track known values held in General Purpose Registers R2 ... R31.

struct memento_t
{
  // One bit for each GPR.
  gprmask_t known = 0;

  std::array<uint8_t, REG_32> values;

  static gprmask_t fixed_regs_mask;

  void apply (const ply_t &);

  void apply_insn (rtx_insn *insn, bool unused)
  {
    apply_insn1 (insn, unused);
    known &= ~memento_t::fixed_regs_mask;
  }

private:
  void apply_insn1 (rtx_insn *, bool);

public:
  bool knows (int rno, int n = 1) const
  {
    gcc_checking_assert (gpr_regno_p (rno, n));
    const gprmask_t mask = regmask (rno, n);
    return (known & mask) == mask;
  }

  uint8_t operator[] (int rno) const
  {
    gcc_checking_assert (gpr_regno_p (rno));
    return values[rno];
  }

  // Set the 8-bit register number DEST as known to hold value VAL.
  void set_value (int dest, int val)
  {
    gcc_checking_assert (gpr_regno_p (dest, 1));
    values[dest] = (uint8_t) val;
    set_known (dest);
  }

  void copy_value (int dest, int src)
  {
    gcc_checking_assert (gpr_regno_p (dest, 1));
    gcc_checking_assert (gpr_regno_p (src, 1));
    values[dest] = values[src];
    set_known (dest, knows (src));
  }

  void copy_values (int dest, int src, int n_bytes)
  {
    gcc_checking_assert (gpr_regno_p (dest, n_bytes));
    gcc_checking_assert (gpr_regno_p (src, n_bytes));
    if (dest < src)
      for (int n = 0; n < n_bytes; ++n)
	copy_value (n + dest, n + src);
    else if (dest > src)
      for (int n = n_bytes - 1; n >= 0; --n)
	copy_value (n + dest, n + src);
  }

  // Get the value as a CONST_INT or NULL_RTX when any byte is unknown.
  rtx get_value_as_const_int (int regno, int n_bytes) const
  {
    gcc_checking_assert (gpr_regno_p (regno, n_bytes));

    if (! knows (regno, n_bytes))
      return NULL_RTX;

    const machine_mode mode = size_to_mode (n_bytes);
    uint64_t val = 0;

    for (int i = n_bytes - 1; i >= 0; --i)
      val = 256 * val + values[regno + i];

    return gen_int_mode (val, mode);
  }

  // Copy the known state and the value (provided it is known) from
  // register SRC to register DEST.
  void copy_values (rtx dest, rtx src)
  {
    if (REG_P (dest) && REG_P (src)
	&& GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (GET_MODE (dest)))
      {
	int n_bytes = std::min (GET_MODE_SIZE (GET_MODE (src)),
				GET_MODE_SIZE (GET_MODE (dest)));
	copy_values (REGNO (dest), REGNO (src), n_bytes);
      }
  }

  void set_values (rtx dest, rtx src)
  {
    gcc_assert (REG_P (dest) && CONST_INT_P (src));
    int regno = REGNO (dest);
    for (int i = 0; i < GET_MODE_SIZE (GET_MODE (dest)); ++i)
      set_value (regno + i, avr_uint8 (src, i));
  }

  // Value >= 0 of the i-th reg or -1 if unknown.
  int value (int i) const
  {
    gcc_checking_assert (gpr_regno_p (i));
    return knows (i) ? (int) values[i] : -1;
  }

  // Value >= 0 of the rno-th reg[n] or -1 if unknown.
  int64_t value (int rno, int n, bool strict = true) const
  {
    gcc_assert (n <= 4);
    gcc_checking_assert (gpr_regno_p (rno, n));
    if (! knows (rno, n))
      {
	if (! strict)
	  return -1;
	gcc_unreachable ();
      }

    uint64_t val = 0;
    for (int r = rno + n - 1; r >= rno; --r)
      val = 256 * val + values[r];

    return val;
  }

  void set_known (int r, bool kno = true)
  {
    gcc_checking_assert (gpr_regno_p (r));
    known = kno
      ? known | (1u << r)
      : known & ~(1u << r);
  }

  void set_unknown (int r)
  {
    gcc_checking_assert (gpr_regno_p (r));
    set_known (r, false);
  }

  int n_known () const
  {
    return popcount_hwi (known);
  }

  // Hamming byte distance of R[n] to VAL.
  int hamming (int r, int n, uint64_t val) const
  {
    gcc_assert (n <= 8);
    gcc_checking_assert (gpr_regno_p (r, n));

    int ham = 0;
    for (int i = 0; i < n; ++i)
      ham += value (r + i) != (uint8_t) (val >> (8 * i));

    return ham;
  }

  // Calculate the Hamming byte distance, ignoring regs in IGNORES.
  int distance_to (const memento_t &that, gprmask_t ignores = 0) const
  {
    int d = 0;
    for (int r = FIRST_GPR; r < REG_32; ++r)
      if (! (ignores & (1u << r)))
	d += value (r) != that.value (r);
    return d;
  }

  // Return true when *this and THAT are the same, with the only allowed
  // exceptions as of mask IGNORES.
  bool equals (const memento_t &that, gprmask_t ignores) const
  {
    if ((known & ~ignores) != (that.known & ~ignores))
      return false;

    for (int r = FIRST_GPR; r < REG_32; ++r)
      if (! (ignores & (1u << r)))
	if (value (r) != that.value (r))
	  return false;

    return true;
  }

  // Return TRUE iff the N_BYTES registers starting at REGNO are known
  // to contain VAL.
  bool have_value (int rno, int n_bytes, int val) const
  {
    gcc_assert (n_bytes <= 4);
    for (int i = rno; i < rno + n_bytes; ++i)
      if (value (i) != (uint8_t) val)
	return false;
      else
	val >>= 8;

    return true;
  }

  // The regno of a d-reg that has a known value, or 0 if none found.
  int known_dregno (void) const
  {
    const gprmask_t dregs = known & 0xffff0000 & ~memento_t::fixed_regs_mask;
    return dregs ? clz_hwi (1) - clz_hwi (dregs) : 0;
  }

  // Return a regno for a register that contains value VAL8 and that does
  // not overlap with the registers mentioned in EXCLUDES.  Else return 0.
  int regno_with_value (uint8_t val8, gprmask_t excludes) const
  {
    for (int r = REG_31; r >= FIRST_GPR; --r)
      if (value (r) == val8
	  && ! (regmask (r, 1) & excludes))
	return r;
    return 0;
  }

  // Return a regno for a 16-bit reg that contains value HI8:LO8 and that does
  // not overlap with the registers mentioned in EXCLUDES.  Else return 0.
  int reg16_with_value (uint8_t lo8, uint8_t hi8, gprmask_t excludes) const
  {
    for (int r = REG_30; r >= FIRST_GPR; r -= 2)
      if (! (regmask (r, 2) & excludes)
	  && value (r) == lo8
	  && value (r + 1) == hi8)
	return r;
    return 0;
  }

  void operator&= (const HARD_REG_SET &hrs)
  {
    known &= elt0_getter<HARD_REG_SET, HARD_REG_ELT_TYPE>::get (hrs);
  }

  // Coalesce register knowledge about *this and THAT.
  void coalesce (const memento_t &that)
  {
    known &= that.known;

    for (int i = FIRST_GPR; i < REG_32; ++i)
      if (values[i] != that.values[i])
	set_unknown (i);
  }

  void dump (const char *msg = nullptr, FILE *f = dump_file) const
  {
    if (f)
      {
	msg = msg && msg[0] ? msg : "%s\n";
	const char *const xs = strstr (msg, "%s");
	gcc_assert (xs);

	fprintf (f, "%.*s", (int) (xs - msg), msg);
	fprintf (f, " (%d known): ", n_known ());
	for (int i = FIRST_GPR; i < REG_32; ++i)
	  if (knows (i))
	    fprintf (f, " r%d=%02x", i, values[i]);

	fprintf (f, "%s", xs + strlen ("%s"));
      }
  }
}; // memento_t


// In avr-fuse-move, a possible step towards an optimal code sequence
// to load a compile-time constant.  A ply_t represents one or two
// instructions.  There are cases where there is no 1-to-1 correspondence
// between a ply_t and an insn; but a sequence of ply_ts can be mapped to
// a sequence of insns; though there are cases where 2 or more ply_ts map
// to a single insn and vice versa.

struct ply_t
{
  // The destination register with .size in { 1, 2 }.
  int regno;
  int size;

  // The performed operation where .arg represents an optional source operand.
  // .code may be one of:  SET (ldi, clr, ldi+mov), REG (mov, movw), NEG (neg),
  // NOT (com), PRE_INC (inc), PRE_DEC (dec), ROTATE (swap), ASHIFT (lsl),
  // LSHIFTRT (lsr), ASHIFTRT (asr), PLUS (add), MINUS (sub), AND (and),
  // IOR (or), XOR (eor), SS_PLUS (adiw, sbiw), MOD (set+bld, clt+bld, bld).
  rtx_code code;
  int arg;

  // Code size in terms of words / instructions.  Extra costs for, say
  // a CLT prior to a sequence of BLDs, are added to the 1st element.
  int cost;

  // We only consider ply_ts that reduce the Hamming distance by 0, 1 or 2.
  // There are exotic cases where the Hamming distance temporarily increases,
  // but we don't consider them.  (They may fall out of the algorithm anyways,
  // for example when a "set_some" insn is used that restores its scratch.
  int dhamming = 1;

  // Whether this is a SET that's intended for insn "set_some"'s payload.
  bool in_set_some = false;

  // 0 or an upper scratch register.  One needed for SETs of a lower reg.
  // SETs in a set_some don't need a scratch.
  int scratch = 0;

  // Statistics.
  static int n_ply_ts;
  static int max_n_ply_ts;

  gprmask_t mask_dest () const
  {
    return regmask (regno, size);
  }

  gprmask_t mask_src () const
  {
    if (code == SET)
      return 0;
    else if (code == REG)
      return regmask (arg, size);
    else if (code == PLUS || code == MINUS || code == AND
	     || code == IOR || code == XOR)
      return regmask (arg, size) | mask_dest ();
    else
      return mask_dest ();
  }

  bool is_movw () const
  {
    return size == 2 && code == REG;
  }

  bool is_adiw () const
  {
    return size == 2 && code == SS_PLUS;
  }

  bool is_bld () const
  {
    return code == MOD;
  }

  // A BLD setting one bit.
  bool is_setbld () const
  {
    return is_bld () && popcount_hwi (arg) == 1;
  }

  // A BLD clearing one bit.
  bool is_cltbld () const
  {
    return is_bld () && popcount_hwi (arg) == 7;
  }

  rtx_code bld_rtx_code () const
  {
    return select<rtx_code>()
      : is_setbld () ? IOR
      : is_cltbld () ? AND
      : UNKNOWN;
  }

  // Is *P a BLD of the same kind?
  bool is_same_bld (const ply_t *p) const
  {
    gcc_assert (is_bld ());
    return p && bld_rtx_code () == p->bld_rtx_code ();
  }

  int bld_bitno () const
  {
    gcc_assert (is_bld ());
    int bit = exact_log2 (popcount_hwi (arg) == 1 ? arg : 0xff ^ arg);
    gcc_assert (IN_RANGE (bit, 0, 7));

    return bit;
  }

  bool needs_scratch () const
  {
    return code == SET && AVRasm::ldi_needs_scratch (regno, arg);
  }

  // Return true when *this modifies (changes *AND* uses) the result
  // generated by *P.
  bool changes_result_of (const ply_t *p) const
  {
    return code != REG && code != SET && (mask_dest() & p->mask_dest());
  }

  bool overrides (const ply_t *p) const
  {
    return code == REG || code == SET
      ? mask_dest () & p->mask_dest ()
      : false;
  }

  bool commutes_with (const ply_t *p, int scratch = 0) const
  {
    if (code == SET || p->code == SET)
      {
	// SETs will be emit as a group where they commute.
	if (code == SET && p->code == SET)
	  return true;

	// Grant more flexibility to move around expensive SETs.
	if (! scratch
	    && (needs_scratch () || p->needs_scratch ()))
	  return false;
      }

    if (is_bld () || p->is_bld ())
      {
	// BLD requires a previous SET or CLT which means that like
	// BLDs should occur as a contiguous sequence.  This limits
	// re-ordering for the purpose of canonicalization of instruction
	// ordering.
	return ((is_cltbld () && p->is_cltbld ())
		|| (is_setbld () && p->is_setbld ()));
      }

    gprmask_t msrc = 1u << scratch;
    gprmask_t m1 = mask_dest() | mask_src();
    gprmask_t m2 = p->mask_dest() | p->mask_src();
    return (m1 & m2) == 0 && ((m1 | m2) & msrc) == 0;
  }

  // Expected insn name; used in dumps.
  const char *insn_name () const
  {
    if (code == SET)
      return select<const char *>()
	: in_set_some ? "set_some"
	: scratch && needs_scratch () ? "*reload_inqi"
	: "movqi_insn";

    return "???";
  }

  void dump (int level = 0, FILE *f = dump_file) const
  {
    if (f)
      {
	if (level)
	  avr_fdump (f, ";; .%d ply_t R%d[%d] = %C", level, regno, size, code);
	else
	  avr_fdump (f, ";; ply_t R%d[%d] = %C", regno, size, code);
	if (code == REG || is_adiw ())
	  fprintf (f, " %d", arg);
	else if (code == PLUS || code == MINUS || code == AND
		 || code == IOR || code == XOR)
	  fprintf (f, " R%d", arg);
	else if (is_setbld ())
	  fprintf (f, " BLD |= 0x%02x", arg);
	else if (is_cltbld ())
	  fprintf (f, " BLD &= 0x%02x", arg);
	else
	  fprintf (f, " 0x%x = %d", arg, arg);

	const char *const name = insn_name ();
	fprintf (f, ", cost=%d, dhamm=%d", cost, dhamming);
	if (name && name[0] != '?')
	  fprintf (f, ", \"%s\"", name);
	fprintf (f, "\n");
      }
  }

  // Helper for dump_plys:  Value of the destination.
  int dest_value (const memento_t &memo) const
  {
    return memo.value (regno, size);
  }

  // Helper for dump_plys:  Value of 1st source arg provided it is a register.
  int src1_value (const memento_t &memo) const
  {
    int rsrc = regno;

    switch (code)
      {
      default:
	return -1;

      case REG:
	gcc_assert (size == 1 || size == 2);
	rsrc = arg;
	break;

      case SS_PLUS:
	gcc_assert (size == 2);
	break;

      case NEG: case NOT: case PRE_DEC: case PRE_INC:
      case ASHIFT: case LSHIFTRT: case ASHIFTRT: case ROTATE:
      case AND: case IOR: case XOR: case MOD:
      case PLUS: case MINUS:
	gcc_assert (size == 1);
	break;
      }

    return memo.value (rsrc, size);
  }

  // Helper for dump_plys:  Value of 2nd source argument.
  int src2_value (const memento_t &memo) const
  {
    switch (code)
      {
      default:
	break;

      case AND: case IOR: case XOR:
      case PLUS: case MINUS:
	gcc_assert (size == 1);

	return memo.value (arg, 1);
      }

    return -1;
  }

  // Dumping a solution (or parts of it) is tedious because when
  // their specific action should be displayed.
  static void dump_plys (FILE *f, int level, int len,
			 const ply_t *const ps[], const memento_t &m0)
  {
    if (f)
      {
	memento_t memo = m0;

	for (int i = 0; i < len; ++i)
	  ps[i]->dump (level, memo, f);
      }
  }

  void dump (int level, memento_t &memo, FILE *f = dump_file) const
  {
    if (! f)
      return;

    const ply_t &p = *this;

    // Keep track of chars in the current line for neat alignment.
    int cs = level > 0
      ? fprintf (f, ";; .%d ", level)
      : fprintf (f, ";; ");
    cs += fprintf (f, "ply_t %-4s R%d[%d] = ", p.mnemonic (), p.regno, p.size);

    const int x = p.src1_value (memo);
    const int y = p.src2_value (memo);

    memo.apply (p);

    const int z = p.dest_value (memo);

    switch (p.code)
      {
      default:
	fprintf (f, "%s ???", rtx_name[p.code]);
	gcc_unreachable ();
	break;

      case REG:
	cs += fprintf (f, "R%d = 0x%0*x", p.arg, 2 * p.size, x);
	break;

      case SET:
	cs += fprintf (f, "0x%02x = %d, \"%s\"", p.arg, p.arg, insn_name ());
	break;

      case PRE_DEC: case PRE_INC:
      case ASHIFT: case LSHIFTRT: case ASHIFTRT: case ROTATE:
	cs += fprintf (f, "R%d %s = 0x%02x = 0x%02x %s",
		       p.regno, p.op_str (), z, x, p.op_str ());
	break;

      case NEG: case NOT:
	cs += fprintf (f, "%sR%d = 0x%02x = %s0x%02x",
		       p.op_str (), p.regno, z, p.op_str (), x);
	break;

      case PLUS: case MINUS:
      case AND: case IOR: case XOR:
	cs += fprintf (f, "R%d %s R%d = 0x%02x = 0x%02x %s 0x%02x",
		       p.regno, p.op_str (), p.arg, z, x, p.op_str (), y);
	break;

      case SS_PLUS: // ADIW / SBIW
	{
	  int arg = (int16_t) p.arg;
	  char op = arg < 0 ? '-' : '+';
	  cs += fprintf (f, "R%d %c %d = 0x%04x = 0x%04x %c %d", p.regno,
			 op, std::abs (arg), z, x, op, std::abs (arg));
	}
	break;

      case MOD: // BLD
	{
	  const char opc = "&|" [p.is_setbld ()];
	  cs += fprintf (f, "R%d %c 0x%02x = 0x%02x = 0x%02x %c bit%d",
			 p.regno, opc, p.arg, z, x, opc, p.bld_bitno ());
	}
	break;
      }

    cs += fprintf (f, ", ");

    while (cs++ < 56)
      fputc (' ', f);

    fprintf (f, "cost=%d, dhamm=%d\n", p.cost, p.dhamming);
  }

  // AVR mnemnic; used in dumps.
  const char *mnemonic () const
  {
    if (is_bld ())
      {
	static char s_bld[] = "BLD*";
	s_bld[3] = '0' + bld_bitno ();
	return s_bld;
      }

    return select<const char *>()
      : code == LSHIFTRT ? "LSR"
      : code == ASHIFTRT ? "ASR"
      : code == ASHIFT ? "LSL"
      : code == ROTATE ? "SWAP"
      : code == PRE_DEC ? "DEC"
      : code == PRE_INC ? "INC"
      : code == MINUS ? "SUB"
      : code == PLUS ? "ADD"
      : code == NEG ? "NEG"
      : code == NOT ? "COM"
      : code == AND ? "AND"
      : code == IOR ? "OR"
      : code == XOR ? "EOR"
      : code == REG ? size == 1 ? "MOV" : "MOVW"
      : code == SET ? arg == 0 ? "CLR" : "LDI"
      : code == SS_PLUS ? arg < 0 ? "SBIW" : "ADIW"
      : rtx_name[code];
  }

  // Return a string of length 1 for CODE, or "?".
  static const char *code_name_str1 (rtx_code code)
  {
    return select<const char *>()
      : code == NEG ? "-"
      : code == NOT ? "~"
      : code == AND ? "&"
      : code == IOR ? "|"
      : code == XOR ? "^"
      : code == PLUS ? "+"
      : code == MINUS ? "-"
      : "?";
  }

  // Short semantics representation used in dumps.
  const char *op_str () const
  {
    return select<const char *>()
      : code == LSHIFTRT ? ">> 1"
      : code == ASHIFTRT ? ">> 1"
      : code == ASHIFT ? "<< 1"
      : code == ROTATE ? ">>> 4"
      : code == PRE_DEC ? "- 1"
      : code == PRE_INC ? "+ 1"
      : code == SS_PLUS ? "+"
      : *(ply_t::code_name_str1 (code)) != '?' ? ply_t::code_name_str1 (code)
      : rtx_name[code];
  }
}; // ply_t


// A set of ply_t's.  We prefer std:array (with some expected upper
// bound for the number of ply_t's as generated by bbinfo_t::get_plies())
// over std::vector.  That way, all plies_t are only allocated once as
// elements of avr_pass_fuse_move::BInfo.

struct plies_t
{
  int n_plies;
  std::array<ply_t, 50> plies;

  int emit_insns (const insninfo_t &, const memento_t &) const;
  int emit_sets (const insninfo_t&, int &n_insns, const memento_t&, int) const;
  int emit_blds (const insninfo_t &, int &n_insns, int i0) const;
  void add_plies_movw (int regno, int size, uint64_t, int, const memento_t &);

  void reset ()
  {
    n_plies = 0;
  }

  void add (const ply_t &ply)
  {
    if (n_plies < (int) plies.size ())
      {
	plies[n_plies++] = ply;
	ply_t::n_ply_ts += 1;
      }
    else
      avr_dump (";; WARNING: plies_t is full\n");
  }

  void add (ply_t, const ply_t *prev, const memento_t &, bool maybe_set_some);

  plies_t () {}

  plies_t (int n, const ply_t *const ps[])
  {
    gcc_assert (n <= (int) plies.size ());
    for (int i = 0; i < n; ++i)
      plies[i] = *ps[i];
    n_plies = n;
  }

  static int max_n_plies;
}; // plies_t


// An 8-bit value leaf of absint_byte_t.
// May be known to equal an 8-bit value.
// May be known to equal the content of an 8-bit GPR.
struct absint_val_t
{
  int16_t val8 = -1;
  int8_t regno = 0;

  absint_val_t () {}

  bool knows_val8 () const
  {
    gcc_assert (IN_RANGE (val8, -1, 0xff));
    return val8 >= 0;
  }

  bool knows_regno () const
  {
    gcc_assert (IN_RANGE (regno, 0, REG_31));
    return regno;
  }

  bool clueless () const
  {
    return ! knows_val8 () && ! knows_regno ();
  }

  gprmask_t reg_mask () const
  {
    return regno ? regmask (regno, 1) : 0;
  }

  void dump (FILE *f = dump_file) const
  {
    if (f)
      {
	if (knows_regno ())
	  fprintf (f, "r%d%s", regno, knows_val8 () ? "=" : "");
	if (knows_val8 ())
	  fprintf (f, "%02x", val8);
	else if (! knows_regno ())
	  fprintf (f, "--");
      }
  }
}; // absint_val_t


// One byte in AbsInt.
class absint_byte_t
{
  // "SET": the value is .x0.
  rtx_code code = UNKNOWN;
  absint_val_t x0;
  absint_val_t x1;

public:

  const absint_val_t &arg (int i) const
  {
    gcc_assert (IN_RANGE (i, 0, arity () - 1));
    return i == 1 ? x1 : x0;
  }

  rtx_code get_code () const
  {
    return code;
  }

  absint_byte_t () {}

  absint_byte_t (absint_val_t x)
    : code(x.clueless () ? UNKNOWN : SET), x0(x)
  {}

  // new = <code> A0  where CODE is a unary operation.
  absint_byte_t (rtx_code c, const absint_byte_t &a0)
    : code(c)
  {
    switch (code)
      {
      default:
	gcc_unreachable ();

      case NOT:
	if (a0.can (CONST_INT))
	  init_val8 (absint_byte_t::eval (code, a0.val8 ()));
	else if (a0.can (REG))
	  x0 = a0.x0;
	else if (a0.can (NOT))
	  init_regno (a0.regno ());
	else
	  code = UNKNOWN;
	break;

      case SIGN_EXTEND:
	if (a0.can (CONST_INT))
	  init_val8 (absint_byte_t::eval (code, a0.val8 ()));
	else if (a0.can (REG))
	  x0 = a0.x0;
	else
	  code = UNKNOWN;
	break;
      }
  }

  // new = A0 <code> A1  where CODE is a binary operation.
  absint_byte_t (rtx_code c, const absint_byte_t &a0, const absint_byte_t &a1)
    : code(c)
  {
    gcc_assert (c == AND || c == IOR || c == XOR || code == PLUS);

    if (a1.is_image1 (c))
      *this = a1;
    else if (a0.is_image1 (c))
      *this = a0;
    else if (a1.is_neutral (c))
      *this = a0;
    else if (a0.is_neutral (c))
      *this = a1;
    else if (a0.can (CONST_INT) && a1.can (CONST_INT))
      init_val8 (absint_byte_t::eval (code, a0.val8 (), a1.val8 ()));
    else if (a0.can (REG) && a1.can (CONST_INT))
      {
	x0 = a0.x0;
	x1 = a1.x0;
	if (code == XOR && a1.val8 () == 0xff)
	  code = NOT;
      }
    else if (a0.can (CONST_INT) && a1.can (REG))
      {
	x0 = a1.x0;
	x1 = a0.x0;
	if (code == XOR && a0.val8 () == 0xff)
	  code = NOT;
      }
    else if (a0.can (REG) && a1.can (REG))
      {
	x0.regno = std::min (a0.regno (), a1.regno ());
	x1.regno = std::max (a0.regno (), a1.regno ());
      }
    else
      code = UNKNOWN;
  }

  int arity () const
  {
    return select<int>()
      : code == UNKNOWN ? 0
      : code == SET || code == NOT || code == SIGN_EXTEND ? 1
      : code == AND || code == IOR || code == XOR || code == PLUS ? 2
      : bad_case<int> ();
  }

  // Return a byte with 8 signs according to code CODE.
  absint_byte_t get_signs (rtx_code ext) const
  {
    return select<absint_byte_t>()
      : ext == ZERO_EXTEND ? absint_byte_t::from_val8 (0)
      : ext == SIGN_EXTEND ? absint_byte_t (SIGN_EXTEND, *this)
      : ext == LSHIFTRT ? absint_byte_t::from_val8 (0)
      : ext == ASHIFTRT ? absint_byte_t (SIGN_EXTEND, *this)
      : bad_case<absint_byte_t> ();
  }

  gprmask_t reg_mask () const
  {
    return select<gprmask_t>()
      : code == SET ? x0.reg_mask ()
      : arity () == 1 ? x0.reg_mask ()
      : arity () == 2 ? x0.reg_mask () | x1.reg_mask ()
      : bad_case<gprmask_t> ();
  }

  bool check () const
  {
    return select<bool>()
      : arity () >= 1 && x0.clueless () ? false
      : arity () == 2 && x1.clueless () ? false
      : true;
  }

  static inline uint8_t eval (rtx_code code, uint8_t x)
  {
    return select<int>()
      : code == NOT ? ~x
      : code == SIGN_EXTEND ? (x >= 0x80 ? 0xff : 0x00)
      : bad_case<int> ();
  }

  static inline uint8_t eval (rtx_code code, uint8_t x, uint8_t y)
  {
    return select<int>()
      : code == AND ? x & y
      : code == IOR ? x | y
      : code == XOR ? x ^ y
      : code == PLUS ? x + y
      : bad_case<int> ();
  }

  bool is_neutral (rtx_code c) const
  {
    return can (CONST_INT) && val8 () == AVRasm::neutral_val (c);
  }

  bool is_image1 (rtx_code c) const
  {
    return can (CONST_INT) && val8 () == AVRasm::image1_val (c);
  }

  bool can (rtx_code c) const
  {
    if (code == SET)
      gcc_assert (IN_RANGE (x0.val8, 0, 0xff) || gpr_regno_p (x0.regno));

    if (c == CONST_INT)
      return code == SET && x0.knows_val8 ();
    else if (c == REG)
      return code == SET && x0.knows_regno ();
    else if (c == VALUE)
      return code != UNKNOWN;
    else if (c == UNKNOWN
	     || c == SET || c == NOT || c == SIGN_EXTEND
	     || c == AND || c == IOR || c == XOR || c == PLUS)
      return code == c;

    gcc_unreachable ();
  }

  // Return the known byte value in 0...0xff, or -1 if unknown and ! STRICT.
  int val8 (bool strict = true) const
  {
    gcc_assert (! strict || code == SET);
    gcc_assert (! strict || can (CONST_INT));
    return can (CONST_INT) ? x0.val8 : -1;
  }

  int regno (bool strict = true) const
  {
    gcc_assert (! strict || code == SET);
    gcc_assert (! strict || can (REG));
    return can (REG) ? x0.regno : 0;
  }

  void init_val8 (int v)
  {
    gcc_assert (IN_RANGE (v, 0, 0xff));
    x0.val8 = v;
    x0.regno = 0;
    code = SET;
  }

  void init_regno (int r)
  {
    gcc_assert (gpr_regno_p (r));
    x0.val8 = -1;
    x0.regno = r;
    code = SET;
  }

  void learn_val8 (int v)
  {
    gcc_assert (IN_RANGE (v, 0, 0xff));
    gcc_assert (code == SET || code == UNKNOWN);
    x0.val8 = v;
    code = SET;
  }

  void learn_regno (int r)
  {
    gcc_assert (gpr_regno_p (r));
    gcc_assert (code == SET || code == UNKNOWN);
    x0.regno = r;
    code = SET;
  }

  static inline absint_byte_t from_val8 (int val, bool strict = true)
  {
    gcc_assert (IN_RANGE (val, -1, 0xff));
    gcc_assert (! strict || val >= 0);
    absint_byte_t b;
    if (val >= 0)
      b.init_val8 (val);

    return  b;
  }

  // Return a SET rtx that can replace the set_src of INSN.
  // Returns BINARY_P or NULL_RTX.
  absint_byte_t find_alternative_binary (const memento_t &memo) const
  {
    gprmask_t excludes = x1.knows_regno () ? regmask (x1.regno, 1) : 0;
    absint_byte_t alt = *this;

    if (arity () == 2
	&& x0.knows_regno ()
	&& x1.knows_val8 ()
	&& (! x1.knows_regno () || x0.regno != x1.regno)
	&& (alt.x1.regno = memo.regno_with_value (x1.val8, excludes)))
      {
	if (dump_flags & TDF_FOLDING)
	  {
	    alt.dump (";; AI.alternative AI=[%s]");
	    dump (" can replace AI=[%s]\n");
	  }

	return alt;
      }

    return absint_byte_t {};
  }

  rtx to_rtx () const
  {
    if (arity () == 2)
      {
	gcc_assert (x0.knows_regno ());
	gcc_assert (x1.knows_regno ());
	rtx op0 = gen_rtx_REG (QImode, x0.regno);
	rtx op1 = gen_rtx_REG (QImode, x1.regno);
	return gen_rtx_fmt_ee (code, QImode, op0, op1);
      }

    gcc_unreachable ();
  }

  void dump (const char *msg = nullptr, FILE *f = dump_file) const
  {
    if (f)
      {
	msg = msg && msg[0] ? msg : "%s";
	const char *const xs = strstr (msg, "%s");
	gcc_assert (xs);

	fprintf (f, "%.*s", (int) (xs - msg), msg);
	if (code == UNKNOWN)
	  fprintf (f, "--");
	else if (code == SET)
	  x0.dump (f);
	else if (code == NOT)
	  {
	    fprintf (f, "~");
	    x0.dump (f);
	  }
	else if (code == SIGN_EXTEND)
	  {
	    fprintf (f, "signs(");
	    x0.dump (f);
	    fprintf (f, ")");
	  }
	else if (arity () == 2)
	  {
	    x0.dump (f);
	    fprintf (f, "%s", ply_t::code_name_str1 (code));
	    x1.dump (f);
	  }
	else
	  gcc_unreachable ();

	fprintf (f, "%s", xs + strlen ("%s"));
      }
  }
}; // absint_byte_t


struct bbinfo_t
{
  // All BBs of the current function.
  static bbinfo_t *bb_info;

  // bbinfo_t holds additional information for this basic block.
  basic_block bb;

  // Known values held in GPRs.
  memento_t regs;

  // Represents the "time" when the value was set.  When we have the choice
  // between several registers to copy from, we use the first (oldest) set.
  // This can avoid copy-chains.
  std::array<int, REG_32> ticks;
  static int tick;

  // Whether according BB is done and optimized.
  bool done;

  static void optimize_one_function (function *func);
  void optimize_one_block (bool &changed);
  void enter ();
  void leave ();

  // Used when finding a best plies_t.  This object is only needed
  // once and can be shared between all basic blocks.
  struct find_plies_data_t
  {
    // These are used by [run_]find_plies()
    const ply_t *ply_stack[N_BEST_PLYS];
    plies_t plies[N_BEST_PLYS];
    plies_t solution;
    // Register knowledge at start of recursive algo.
    memento_t regs0;
    int max_ply_cost;
    int movmode_cost;
    int n_best_plys;
    int n_get_plies; // Only for bookkeeping / statistics.
  }; // find_plies_data_t

  static find_plies_data_t *fpd;
  static bool try_fuse_p;
  static bool try_mem0_p;
  static bool try_bin_arg1_p;
  static bool try_simplify_p;
  static bool try_split_ldi_p;
  static bool try_split_any_p;
  static bool use_arith_p;
  static bool use_set_some_p;

  static void get_plies (plies_t &, const insninfo_t &, const memento_t &,
			 const ply_t *);
  static void find_plies (int depth, const insninfo_t &, const memento_t &);
  bool run_find_plies (const insninfo_t &, const memento_t &) const;
}; // bbinfo_t
