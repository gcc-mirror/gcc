/* Machine description patterns for PowerPC running Darwin (Mac OS X).
   Copyright (C) 2004-2023 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

This file is part of GCC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

(define_insn "adddi3_high"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=b")
        (plus:DI (match_operand:DI 1 "gpc_reg_operand" "b")
                 (high:DI (match_operand 2 "" ""))))]
  "TARGET_MACHO && TARGET_64BIT"
  "addis %0,%1,ha16(%2)")

(define_insn "movdf_low_si"
  [(set (match_operand:DF 0 "gpc_reg_operand" "=f,!r")
        (mem:DF (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" ""))))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && !TARGET_64BIT"
{
  switch (which_alternative)
    {
      case 0:
	return "lfd %0,lo16(%2)(%1)";
      case 1:
	{
	  if (TARGET_POWERPC64 && TARGET_32BIT)
	    /* Note, old assemblers didn't support relocation here.  */
	    return "ld %0,lo16(%2)(%1)";
	  else
	    {
	      output_asm_insn ("la %0,lo16(%2)(%1)", operands);
	      output_asm_insn ("lwz %L0,4(%0)", operands);
	      return ("lwz %0,0(%0)");
	    }
	}
      default:
	gcc_unreachable ();
    }
}
  [(set_attr "type" "load")
   (set_attr "length" "4,12")])


(define_insn "movdf_low_di"
  [(set (match_operand:DF 0 "gpc_reg_operand" "=f,!r")
        (mem:DF (lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" ""))))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && TARGET_64BIT"
  "@
   lfd %0,lo16(%2)(%1)
   ld %0,lo16(%2)(%1)"
  [(set_attr "type" "load")])

(define_insn "movdf_low_st_si"
  [(set (mem:DF (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "b")
                           (match_operand 2 "" "")))
	(match_operand:DF 0 "gpc_reg_operand" "f"))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && ! TARGET_64BIT"
  "stfd %0,lo16(%2)(%1)"
  [(set_attr "type" "store")])

(define_insn "movdf_low_st_di"
  [(set (mem:DF (lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b")
                           (match_operand 2 "" "")))
	(match_operand:DF 0 "gpc_reg_operand" "f"))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && TARGET_64BIT"
  "stfd %0,lo16(%2)(%1)"
  [(set_attr "type" "store")])

(define_insn "movsf_low_si"
  [(set (match_operand:SF 0 "gpc_reg_operand" "=f,!r")
        (mem:SF (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" ""))))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && ! TARGET_64BIT"
  "@
   lfs %0,lo16(%2)(%1)
   lwz %0,lo16(%2)(%1)"
  [(set_attr "type" "load")])

(define_insn "movsf_low_di"
  [(set (match_operand:SF 0 "gpc_reg_operand" "=f,!r")
        (mem:SF (lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" ""))))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && TARGET_64BIT"
  "@
   lfs %0,lo16(%2)(%1)
   lwz %0,lo16(%2)(%1)"
  [(set_attr "type" "load")])

(define_insn "movsf_low_st_si"
  [(set (mem:SF (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" "")))
	(match_operand:SF 0 "gpc_reg_operand" "f,!r"))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && ! TARGET_64BIT"
  "@
   stfs %0,lo16(%2)(%1)
   stw %0,lo16(%2)(%1)"
  [(set_attr "type" "store")])

(define_insn "movsf_low_st_di"
  [(set (mem:SF (lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" "")))
	(match_operand:SF 0 "gpc_reg_operand" "f,!r"))]
  "TARGET_MACHO && TARGET_HARD_FLOAT && TARGET_64BIT"
  "@
   stfs %0,lo16(%2)(%1)
   stw %0,lo16(%2)(%1)"
  [(set_attr "type" "store")])

;; Mach-O PIC.

(define_insn "@macho_high_<mode>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=b*r")
	(high:P (match_operand 1 "" "")))]
  "TARGET_MACHO && (DEFAULT_ABI == ABI_DARWIN) && !flag_pic"
  "lis %0,ha16(%1)")

(define_insn "@macho_low_<mode>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=r")
	(lo_sum:P (match_operand:P 1 "gpc_reg_operand" "b")
		   (match_operand 2 "" "")))]
   "TARGET_MACHO && (DEFAULT_ABI == ABI_DARWIN) && !flag_pic"
   "la %0,lo16(%2)(%1)")

(define_insn "@machopic_high_<mode>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=b*r")
	(high:P (match_operand 1 "macho_pic_address" "")))]
  "TARGET_MACHO && flag_pic"
  "lis %0,ha16(%1)")

(define_insn "@machopic_low_<mode>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=r")
	(lo_sum:P (match_operand:P 1 "gpc_reg_operand" "b")
		   (match_operand 2 "macho_pic_address" "")))]
   "TARGET_MACHO && flag_pic"
   "la %0,lo16(%2)(%1)")

(define_split
  [(set (mem:V4SI (plus:DI (match_operand:DI 0 "gpc_reg_operand")
			 (match_operand:DI 1 "short_cint_operand")))
	(match_operand:V4SI 2 "register_operand"))
   (clobber (match_operand:DI 3 "gpc_reg_operand"))]
  "TARGET_MACHO && TARGET_64BIT"
  [(set (match_dup 3) (plus:DI (match_dup 0) (match_dup 1)))
   (set (mem:V4SI (match_dup 3))
	(match_dup 2))]
  "")

(define_insn "@macho_correct_pic_<mode>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=r")
	(plus:P (match_operand:P 1 "gpc_reg_operand" "r")
		 (unspec:P [(match_operand:P 2 "immediate_operand" "s")
			     (match_operand:P 3 "immediate_operand" "s")]
			    UNSPEC_MPIC_CORRECT)))]
  "DEFAULT_ABI == ABI_DARWIN"
  "addis %0,%1,ha16(%2-%3)\n\taddi %0,%0,lo16(%2-%3)"
  [(set_attr "length" "8")])

(define_insn "@load_macho_picbase_<mode>"
  [(set (reg:P LR_REGNO)
	(unspec:P [(match_operand:P 0 "immediate_operand" "s")
		    (pc)] UNSPEC_LD_MPIC))]
  "(DEFAULT_ABI == ABI_DARWIN) && flag_pic"
{
#if TARGET_MACHO
  machopic_should_output_picbase_label (); /* Update for new func.  */
#else
  gcc_unreachable ();
#endif
  return "bcl 20,31,%0\n%0:";
}
  [(set_attr "type" "branch")
   (set_attr "cannot_copy" "yes")])

(define_insn "@reload_macho_picbase_<mode>"
  [(set (reg:P LR_REGNO)
        (unspec:P [(match_operand:P 0 "immediate_operand" "s")
		    (pc)] UNSPEC_RELD_MPIC))]
  "(DEFAULT_ABI == ABI_DARWIN) && flag_pic"
{
#if TARGET_MACHO
  if (machopic_should_output_picbase_label ())
    {
      static char tmp[64];
      const char *cnam = machopic_get_function_picbase ();
      snprintf (tmp, 64, "bcl 20,31,%s\n%s:\n%%0:", cnam, cnam);
      return tmp;
    }
  else
#else
  gcc_unreachable ();
#endif
    return "bcl 20,31,%0\n%0:";
}
  [(set_attr "type" "branch")
   (set_attr "cannot_copy" "yes")])

;; We need to restore the PIC register, at the site of nonlocal label.

(define_insn_and_split "nonlocal_goto_receiver"
  [(unspec_volatile [(const_int 0)] UNSPECV_NLGR)]
  "TARGET_MACHO && flag_pic"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
#if TARGET_MACHO
  if (crtl->uses_pic_offset_table)
    {
      static unsigned n = 0;
      rtx picrtx = gen_rtx_SYMBOL_REF (Pmode, MACHOPIC_FUNCTION_BASE_NAME);
      rtx picreg = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);
      rtx tmplrtx;
      char tmplab[20];

      ASM_GENERATE_INTERNAL_LABEL(tmplab, "Lnlgr", ++n);
      tmplrtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (tmplab));

      emit_insn (gen_reload_macho_picbase (Pmode, tmplrtx));
      emit_move_insn (picreg, gen_rtx_REG (Pmode, LR_REGNO));
      emit_insn (gen_macho_correct_pic (Pmode, picreg, picreg,
					picrtx, tmplrtx));
    }
  else
    /* Not using PIC reg, no reload needed.  */
    emit_note (NOTE_INSN_DELETED);
#else
  gcc_unreachable ();
#endif
  DONE;
})
