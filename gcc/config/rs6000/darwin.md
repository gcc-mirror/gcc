/* Machine description patterns for PowerPC running Darwin (Mac OS X).
   Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

;; 64-bit MachO load/store support
(define_insn "movdi_low"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=r,*!d")
        (mem:DI (lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" ""))))]
  "TARGET_MACHO && TARGET_64BIT"
  "@
   ld %0,lo16(%2)(%1)
   lfd %0,lo16(%2)(%1)"
  [(set_attr "type" "load")])

(define_insn "movsi_low_st"
  [(set (mem:SI (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "b")
                           (match_operand 2 "" "")))
	(match_operand:SI 0 "gpc_reg_operand" "r"))]
  "TARGET_MACHO && ! TARGET_64BIT"
  "stw %0,lo16(%2)(%1)"
  [(set_attr "type" "store")])

(define_insn "movdi_low_st"
  [(set (mem:DI (lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b,b")
                           (match_operand 2 "" "")))
	(match_operand:DI 0 "gpc_reg_operand" "r,*!d"))]
  "TARGET_MACHO && TARGET_64BIT"
  "@
   std %0,lo16(%2)(%1)
   stfd %0,lo16(%2)(%1)"
  [(set_attr "type" "store")])

;; Mach-O PIC trickery.
(define_expand "macho_high"
  [(set (match_operand 0 "")
	(high (match_operand 1 "")))]
  "TARGET_MACHO"
{
  if (TARGET_64BIT)
    emit_insn (gen_macho_high_di (operands[0], operands[1]));
  else
    emit_insn (gen_macho_high_si (operands[0], operands[1]));

  DONE;
})

(define_insn "macho_high_si"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=b*r")
	(high:SI (match_operand 1 "" "")))]
  "TARGET_MACHO && ! TARGET_64BIT"
  "lis %0,ha16(%1)")
  

(define_insn "macho_high_di"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=b*r")
	(high:DI (match_operand 1 "" "")))]
  "TARGET_MACHO && TARGET_64BIT"
  "lis %0,ha16(%1)")

(define_expand "macho_low"
  [(set (match_operand 0 "")
	(lo_sum (match_operand 1 "")
		   (match_operand 2 "")))]
   "TARGET_MACHO"
{
  if (TARGET_64BIT)
    emit_insn (gen_macho_low_di (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_macho_low_si (operands[0], operands[1], operands[2]));

  DONE;
})

(define_insn "macho_low_si"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "b")
		   (match_operand 2 "" "")))]
   "TARGET_MACHO && ! TARGET_64BIT"
   "la %0,lo16(%2)(%1)")

(define_insn "macho_low_di"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=r")
	(lo_sum:DI (match_operand:DI 1 "gpc_reg_operand" "b")
		   (match_operand 2 "" "")))]
   "TARGET_MACHO && TARGET_64BIT"
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

(define_expand "load_macho_picbase"
  [(set (reg:SI LR_REGNO)
        (unspec [(match_operand 0 "")]
                   UNSPEC_LD_MPIC))]
  "(DEFAULT_ABI == ABI_DARWIN) && flag_pic"
{
  if (TARGET_32BIT)
    emit_insn (gen_load_macho_picbase_si (operands[0]));
  else
    emit_insn (gen_load_macho_picbase_di (operands[0]));

  DONE;
})

(define_insn "load_macho_picbase_si"
  [(set (reg:SI LR_REGNO)
	(unspec:SI [(match_operand:SI 0 "immediate_operand" "s")
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

(define_insn "load_macho_picbase_di"
  [(set (reg:DI LR_REGNO)
	(unspec:DI [(match_operand:DI 0 "immediate_operand" "s")
		    (pc)] UNSPEC_LD_MPIC))]
  "(DEFAULT_ABI == ABI_DARWIN) && flag_pic && TARGET_64BIT"
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

(define_expand "macho_correct_pic"
  [(set (match_operand 0 "")
	(plus (match_operand 1 "")
		 (unspec [(match_operand 2 "")
			     (match_operand 3 "")]
			    UNSPEC_MPIC_CORRECT)))]
  "DEFAULT_ABI == ABI_DARWIN"
{
  if (TARGET_32BIT)
    emit_insn (gen_macho_correct_pic_si (operands[0], operands[1], operands[2],
	       operands[3]));
  else
    emit_insn (gen_macho_correct_pic_di (operands[0], operands[1], operands[2],
	       operands[3]));

  DONE;
})

(define_insn "macho_correct_pic_si"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r")
	(plus:SI (match_operand:SI 1 "gpc_reg_operand" "r")
		 (unspec:SI [(match_operand:SI 2 "immediate_operand" "s")
			     (match_operand:SI 3 "immediate_operand" "s")]
			    UNSPEC_MPIC_CORRECT)))]
  "DEFAULT_ABI == ABI_DARWIN"
  "addis %0,%1,ha16(%2-%3)\n\taddi %0,%0,lo16(%2-%3)"
  [(set_attr "length" "8")])

(define_insn "macho_correct_pic_di"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=r")
	(plus:DI (match_operand:DI 1 "gpc_reg_operand" "r")
		 (unspec:DI [(match_operand:DI 2 "immediate_operand" "s")
			     (match_operand:DI 3 "immediate_operand" "s")]
			    16)))]
  "DEFAULT_ABI == ABI_DARWIN && TARGET_64BIT"
  "addis %0,%1,ha16(%2-%3)\n\taddi %0,%0,lo16(%2-%3)"
  [(set_attr "length" "8")])

(define_insn "*call_indirect_nonlocal_darwin64"
  [(call (mem:SI (match_operand:DI 0 "register_operand" "c,*l,c,*l"))
	 (match_operand 1 "" "g,g,g,g"))
   (use (match_operand:SI 2 "immediate_operand" "O,O,n,n"))
   (clobber (reg:SI LR_REGNO))]
  "DEFAULT_ABI == ABI_DARWIN && TARGET_64BIT"
{
  return "b%T0l";
}
  [(set_attr "type" "jmpreg,jmpreg,jmpreg,jmpreg")
   (set_attr "length" "4,4,8,8")])

(define_insn "*call_nonlocal_darwin64"
  [(call (mem:SI (match_operand:DI 0 "symbol_ref_operand" "s,s"))
	 (match_operand 1 "" "g,g"))
   (use (match_operand:SI 2 "immediate_operand" "O,n"))
   (clobber (reg:SI LR_REGNO))]
  "(DEFAULT_ABI == ABI_DARWIN)
   && (INTVAL (operands[2]) & CALL_LONG) == 0"
{
#if TARGET_MACHO
  return output_call(insn, operands, 0, 2);
#else
  gcc_unreachable ();
#endif
}
  [(set_attr "type" "branch,branch")
   (set_attr "length" "4,8")])

(define_insn "*call_value_indirect_nonlocal_darwin64"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:DI 1 "register_operand" "c,*l,c,*l"))
	      (match_operand 2 "" "g,g,g,g")))
   (use (match_operand:SI 3 "immediate_operand" "O,O,n,n"))
   (clobber (reg:SI LR_REGNO))]
  "DEFAULT_ABI == ABI_DARWIN"
{
  return "b%T1l";
}
  [(set_attr "type" "jmpreg,jmpreg,jmpreg,jmpreg")
   (set_attr "length" "4,4,8,8")])

(define_insn "*call_value_nonlocal_darwin64"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:DI 1 "symbol_ref_operand" "s,s"))
	      (match_operand 2 "" "g,g")))
   (use (match_operand:SI 3 "immediate_operand" "O,n"))
   (clobber (reg:SI LR_REGNO))]
  "(DEFAULT_ABI == ABI_DARWIN)
   && (INTVAL (operands[3]) & CALL_LONG) == 0"
{
#if TARGET_MACHO
  return output_call(insn, operands, 1, 3);
#else
  gcc_unreachable ();
#endif
}
  [(set_attr "type" "branch,branch")
   (set_attr "length" "4,8")])

(define_expand "reload_macho_picbase"
  [(set (reg:SI LR_REGNO)
        (unspec [(match_operand 0 "")]
                   UNSPEC_RELD_MPIC))]
  "(DEFAULT_ABI == ABI_DARWIN) && flag_pic"
{
  if (TARGET_32BIT)
    emit_insn (gen_reload_macho_picbase_si (operands[0]));
  else
    emit_insn (gen_reload_macho_picbase_di (operands[0]));

  DONE;
})

(define_insn "reload_macho_picbase_si"
  [(set (reg:SI LR_REGNO)
        (unspec:SI [(match_operand:SI 0 "immediate_operand" "s")
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

(define_insn "reload_macho_picbase_di"
  [(set (reg:DI LR_REGNO)
	(unspec:DI [(match_operand:DI 0 "immediate_operand" "s")
		    (pc)] UNSPEC_RELD_MPIC))]
  "(DEFAULT_ABI == ABI_DARWIN) && flag_pic && TARGET_64BIT"
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

      emit_insn (gen_reload_macho_picbase (tmplrtx));
      emit_move_insn (picreg, gen_rtx_REG (Pmode, LR_REGNO));
      emit_insn (gen_macho_correct_pic (picreg, picreg, picrtx, tmplrtx));
    }
  else
    /* Not using PIC reg, no reload needed.  */
    emit_note (NOTE_INSN_DELETED);
#else
  gcc_unreachable ();
#endif
  DONE;
})
