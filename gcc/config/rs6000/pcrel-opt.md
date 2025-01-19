;; Machine description for the PCREL_OPT optimization.
;; Copyright (C) 2020-2025 Free Software Foundation, Inc.
;; Contributed by Michael Meissner (meissner@linux.ibm.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Support for the PCREL_OPT optimization.  PCREL_OPT looks for instances where
;; an external variable is used only once, either for reading or for writing.
;;
;; If we are optimizing a single read, normally the code would look like:
;;
;;	(set (reg:DI <ptr>)
;;	     (symbol_ref:DI "<extern_addr>"))	# <data> is currently dead
;;
;;		...	# insns do not need to be adjacent
;;
;;	(set (reg:SI <data>)
;;	     (mem:SI (reg:DI <xxx>)))		# <ptr> dies with this insn
;;
;; We transform this into:
;;
;;	(parallel [(set (reg:DI <ptr>)
;;			(unspec:SI [(symbol_ref:DI <extern_addr>)
;;				    (const_int <marker>)]
;;				   UNSPEC_PCREL_OPT_LD_ADDR))
;;		   (set (reg:DI <data>)
;;			(unspec:DI [(const_int 0)]
;;				   UNSPEC_PCREL_OPT_LD_DATA))])
;;
;;	...
;;
;;	(parallel [(set (reg:SI <data>)
;;		   (unspec:SI [(mem:SI (reg:DI <ptr>))
;;			       (reg:DI <data>)
;;			       (const_int <marker>)]
;;			      UNSPEC_PCREL_OPT_LD_RELOC))
;;		   (clobber (reg:DI <ptr>))])
;;
;; The marker is an integer constant that links the load of the external
;; address to the load of the actual variable.
;;
;; In the first insn, we set both the address of the external variable, and
;; mark that the variable being loaded both are created in that insn, and are
;; consumed in the second insn.	 It doesn't matter what mode the register that
;; we will ultimately do the load into, so we use DImode.  We just need to mark
;; that both registers may be set in the first insn, and will be used in the
;; second insn.
;;
;; Since we use UNSPEC's and link both the register holding the external
;; address and the value being loaded, it should prevent other passes from
;; modifying it.
;;
;; If the register being loaded is the same as the base register, we use an
;; alternate form of the insns.
;;
;;	(set (reg:DI <data_ptr>)
;;	     (unspec:DI [(symbol_ref:DI <extern_addr>)
;;			 (const_int <marker>)]
;;			UNSPEC_PCREL_OPT_LD_SAME_REG))
;;
;;	...
;;
;;	(parallel [(set (reg:SI <data>)
;;		   (unspec:SI [(mem:SI (reg:DI <ptr>))
;;			       (reg:DI <data>)
;;			       (const_int <marker>)]
;;			      UNSPEC_PCREL_OPT_LD_RELOC))
;;		   (clobber (reg:DI <ptr>))])

(define_c_enum "unspec"
  [UNSPEC_PCREL_OPT_LD_ADDR
   UNSPEC_PCREL_OPT_LD_DATA
   UNSPEC_PCREL_OPT_LD_SAME_REG
   UNSPEC_PCREL_OPT_LD_RELOC
   UNSPEC_PCREL_OPT_ST_ADDR
   UNSPEC_PCREL_OPT_ST_RELOC])

;; Modes that are supported for PCREL_OPT
(define_mode_iterator PCRELOPT [QI HI SI DI TI SF DF KF
			  V1TI V2DI V4SI V8HI V16QI V2DF V4SF
			  (TF "TARGET_FLOAT128_TYPE && TARGET_IEEEQUAD")])

;; Vector modes for PCREL_OPT
(define_mode_iterator PCRELOPT_VECT [TI KF V1TI V2DI V4SI V8HI V16QI V2DF V4SF
			       (TF "TARGET_FLOAT128_TYPE && TARGET_IEEEQUAD")])

;; Insn for loading the external address, where the register being loaded is not
;; the same as the register being loaded with the data.
(define_insn "pcrel_opt_ld_addr"
  [(set (match_operand:DI 0 "base_reg_operand" "=&b,&b")
	(unspec:DI [(match_operand:DI 1 "pcrel_external_address")
		    (match_operand 2 "const_int_operand" "n,n")]
		   UNSPEC_PCREL_OPT_LD_ADDR))
   (set (match_operand:DI 3 "gpc_reg_operand" "=r,wa")
	(unspec:DI [(const_int 0)]
		   UNSPEC_PCREL_OPT_LD_DATA))]
  "TARGET_PCREL_OPT
   && reg_or_subregno (operands[0]) != reg_or_subregno (operands[3])"
  "ld %0,%a1\n.Lpcrel%2:"
  [(set_attr "prefixed" "yes")
   (set_attr "type" "load")
   (set_attr "loads_external_address" "yes")])

;; Alternate form of loading up the external address that is the same register
;; as the final load.
(define_insn "pcrel_opt_ld_addr_same_reg"
  [(set (match_operand:DI 0 "base_reg_operand" "=b")
	(unspec:DI [(match_operand:DI 1 "pcrel_external_address")
		    (match_operand 2 "const_int_operand" "n")]
		   UNSPEC_PCREL_OPT_LD_SAME_REG))]
  "TARGET_PCREL_OPT"
  "ld %0,%a1\n.Lpcrel%2:"
  [(set_attr "prefixed" "yes")
   (set_attr "type" "load")
   (set_attr "loads_external_address" "yes")])

;; PCREL_OPT modes that are optimized for loading or storing GPRs.
(define_mode_iterator PCRELOPT_GPR [QI HI SI DI SF DF])

(define_mode_attr PCRELOPT_GPR_LD [(QI "lbz")
			     (HI "lhz")
			     (SI "lwz")
			     (SF "lwz")
			     (DI "ld")
			     (DF "ld")])

;; PCREL_OPT load operation of GPRs.  Operand 4 (the register used to hold the
;; address of the external symbol) is SCRATCH if the same register is used for
;; the normal load.
(define_insn "*pcrel_opt_ld<mode>_gpr"
  [(parallel [(set (match_operand:PCRELOPT_GPR 0 "int_reg_operand" "+r")
		   (unspec:PCRELOPT_GPR [
		     (match_operand:PCRELOPT_GPR 1 "d_form_memory" "m")
		     (match_operand:DI 2 "int_reg_operand" "0")
		     (match_operand 3 "const_int_operand" "n")]
		    UNSPEC_PCREL_OPT_LD_RELOC))
	      (clobber (match_scratch:DI 4 "=bX"))])]
  "TARGET_PCREL_OPT
   && (GET_CODE (operands[4]) == SCRATCH
       || reg_mentioned_p (operands[4], operands[1]))"
{
  output_pcrel_opt_reloc (operands[3]);
  return "<PCRELOPT_GPR_LD> %0,%1";
}
  [(set_attr "type" "load")])

;; PCREL_OPT load with sign/zero extension
(define_insn "*pcrel_opt_ldsi_<u><mode>_gpr"
  [(set (match_operand:EXTSI 0 "int_reg_operand" "+r")
	(any_extend:EXTSI
	 (unspec:SI [(match_operand:SI 1 "d_form_memory" "m")
		     (match_operand:DI 2 "int_reg_operand" "0")
		     (match_operand 3 "const_int_operand" "n")]
		     UNSPEC_PCREL_OPT_LD_RELOC)))
   (clobber (match_scratch:DI 4 "=bX"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[3]);
  return "lw<az> %0,%1";
}
  [(set_attr "type" "load")])

(define_insn "*pcrel_opt_ldhi_<u><mode>_gpr"
  [(set (match_operand:EXTHI 0 "int_reg_operand" "+r")
	(any_extend:EXTHI
	 (unspec:HI [(match_operand:HI 1 "d_form_memory" "m")
		     (match_operand:DI 2 "int_reg_operand" "0")
		     (match_operand 3 "const_int_operand" "n")]
		     UNSPEC_PCREL_OPT_LD_RELOC)))
   (clobber (match_scratch:DI 4 "=bX"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[3]);
  return "lh<az> %0,%1";
}
  [(set_attr "type" "load")])

(define_insn "*pcrel_opt_ldqi_u<mode>_gpr"
  [(set (match_operand:EXTQI 0 "int_reg_operand" "+r")
	(zero_extend:EXTQI
	 (unspec:QI [(match_operand:QI 1 "d_form_memory" "m")
		     (match_operand:DI 2 "int_reg_operand" "0")
		     (match_operand 3 "const_int_operand" "n")]
		     UNSPEC_PCREL_OPT_LD_RELOC)))
   (clobber (match_scratch:DI 4 "=bX"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[3]);
  return "lbz %0,%1";
}
  [(set_attr "type" "load")])

;; Scalar types that can be optimized by loading them into floating point
;; or Altivec registers.
(define_mode_iterator PCRELOPT_FP [DI DF SF])

;; Load instructions to load up scalar floating point or 64-bit integer values
;; into floating point registers or Altivec registers.
(define_mode_attr PCRELOPT_FPR_LD [(DI "lfd") (DF "lfd") (SF "lfs")])
(define_mode_attr PCRELOPT_VMX_LD [(DI "lxsd") (DF "lxsd") (SF "lxssp")])

;; PCREL_OPT load operation of scalar DF/DI/SF into vector registers.
(define_insn "*pcrel_opt_ld<mode>_vsx"
  [(set (match_operand:PCRELOPT_FP 0 "vsx_register_operand" "+d,v")
	(unspec:PCRELOPT_FP [(match_operand:PCRELOPT_FP 1 "d_form_memory" "m,m")
		       (match_operand:DI 2 "vsx_register_operand" "0,0")
		       (match_operand 3 "const_int_operand" "n,n")]
		       UNSPEC_PCREL_OPT_LD_RELOC))
   (clobber (match_operand:DI 4 "base_reg_operand" "=b,b"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[3]);
  return which_alternative ? "<PCRELOPT_VMX_LD> %0,%1"
			 : "<PCRELOPT_FPR_LD> %0,%1";
}
  [(set_attr "type" "fpload")])

;; PCREL_OPT optimization extending SFmode to DFmode via a load.
(define_insn "*pcrel_opt_ldsf_df"
  [(set (match_operand:DF 0 "vsx_register_operand" "+d,v")
	(float_extend:DF
	 (unspec:SF [(match_operand:SF 1 "d_form_memory" "m,m")
		     (match_operand:DI 2 "vsx_register_operand" "0,0")
		     (match_operand 3 "const_int_operand" "n,n")]
		    UNSPEC_PCREL_OPT_LD_RELOC)))
   (clobber (match_operand:DI 4 "base_reg_operand" "=b,b"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[3]);
  return which_alternative ? "lxssp %0,%1" : "lfs %0,%1";
}
  [(set_attr "type" "fpload")])

;; PCREL_OPT load operation of vector/float128 types into vector registers.
(define_insn "*pcrel_opt_ld<mode>"
  [(set (match_operand:PCRELOPT_VECT 0 "vsx_register_operand" "+wa")
	(unspec:PCRELOPT_VECT [(match_operand:PCRELOPT_VECT 1 "d_form_memory" "m")
			 (match_operand:DI 2 "vsx_register_operand" "0")
			 (match_operand 3 "const_int_operand" "n")]
			UNSPEC_PCREL_OPT_LD_RELOC))
   (clobber (match_operand:DI 4 "base_reg_operand" "=b"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[3]);
  return "lxv %x0,%1";
}
  [(set_attr "type" "vecload")])


;; PCREL_OPT optimization for stores.  We need to put the label after the PLD
;; instruction, because the assembler might insert a NOP before the PLD for
;; alignment.
;;
;; If we are optimizing a single write, normally the code would look like:
;;
;;	(set (reg:DI <ptr>)
;;	     (symbol_ref:DI "<extern_addr>"))	# <data> must be live here
;;
;;	    ...		     # insns do not need to be adjacent
;;
;;	(set (mem:SI (reg:DI <xxx>))
;;	     (reg:SI <data>))			# <ptr> dies with this insn
;;
;; We optimize this to be:
;;
;;	(parallel [(set (reg:DI <ptr>)
;;			(unspec:DI [(symbol_ref:DI "<extern_addr>")
;;				    (const_int <marker>)]
;;				   UNSPEC_PCREL_OPT_ST_ADDR))
;;		   (use (reg:<MODE> <data>))])
;;
;;	    ...		     # insns do not need to be adjacent
;;
;;	(parallel [(set (mem:<MODE> (reg:DI <ptr>))
;;			(unspec:<MODE> [(reg:<MODE> <data>)
;;					(const_int <marker>)]
;;				       UNSPEC_PCREL_OPT_ST_RELOC))
;;		   (clobber (reg:DI <ptr>))])

(define_insn "*pcrel_opt_st_addr<mode>"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=b")
	(unspec:DI [(match_operand:DI 1 "pcrel_external_address")
		    (match_operand 2 "const_int_operand" "n")]
		UNSPEC_PCREL_OPT_ST_ADDR))
   (use (match_operand:PCRELOPT 3 "gpc_reg_operand" "rwa"))]
  "TARGET_PCREL_OPT"
  "ld %0,%a1\n.Lpcrel%2:"
  [(set_attr "prefixed" "yes")
   (set_attr "type" "load")
   (set_attr "loads_external_address" "yes")])

;; PCREL_OPT stores.
(define_insn "*pcrel_opt_st<mode>"
  [(set (match_operand:QHSI 0 "d_form_memory" "=m")
	(unspec:QHSI [(match_operand:QHSI 1 "gpc_reg_operand" "r")
		      (match_operand 2 "const_int_operand" "n")]
		     UNSPEC_PCREL_OPT_ST_RELOC))
   (clobber (match_operand:DI 3 "base_reg_operand" "=b"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[2]);
  return "st<wd> %1,%0";
}
  [(set_attr "type" "store")])

(define_insn "*pcrel_opt_stdi"
  [(set (match_operand:DI 0 "d_form_memory" "=m,m,m")
	(unspec:DI [(match_operand:DI 1 "gpc_reg_operand" "r,d,v")
		    (match_operand 2 "const_int_operand" "n,n,n")]
		   UNSPEC_PCREL_OPT_ST_RELOC))
   (clobber (match_operand:DI 3 "base_reg_operand" "=b,b,b"))]
  "TARGET_PCREL_OPT && TARGET_POWERPC64"
{
  output_pcrel_opt_reloc (operands[2]);
  switch (which_alternative)
    {
    case 0:
      return "std %1,%0";
    case 1:
      return "stfd %1,%0";
    case 2:
      return "stxsd %1,%0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "store,fpstore,fpstore")])

(define_insn "*pcrel_opt_stsf"
  [(set (match_operand:SF 0 "d_form_memory" "=m,m,m")
	(unspec:SF [(match_operand:SF 1 "gpc_reg_operand" "d,v,r")
		    (match_operand 2 "const_int_operand" "n,n,n")]
		   UNSPEC_PCREL_OPT_ST_RELOC))
   (clobber (match_operand:DI 3 "base_reg_operand" "=b,b,b"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[2]);
  switch (which_alternative)
    {
    case 0:
      return "stfs %1,%0";
    case 1:
      return "stxssp %1,%0";
    case 2:
      return "stw %1,%0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "fpstore,fpstore,store")])

(define_insn "*pcrel_opt_stdf"
  [(set (match_operand:DF 0 "d_form_memory" "=m,m,m")
	(unspec:DF [(match_operand:DF 1 "gpc_reg_operand" "d,v,r")
		    (match_operand 2 "const_int_operand" "n,n,n")]
		   UNSPEC_PCREL_OPT_ST_RELOC))
   (clobber (match_operand:DI 3 "base_reg_operand" "=b,b,b"))]
  "TARGET_PCREL_OPT
   && (TARGET_POWERPC64 || vsx_register_operand (operands[1], DFmode))"
{
  output_pcrel_opt_reloc (operands[2]);
  switch (which_alternative)
    {
    case 0:
      return "stfd %1,%0";
    case 1:
      return "stxsd %1,%0";
    case 2:
      return "std %1,%0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "fpstore,fpstore,store")])

(define_insn "*pcrel_opt_st<mode>"
  [(set (match_operand:PCRELOPT_VECT 0 "d_form_memory" "=m")
	(unspec:PCRELOPT_VECT [(match_operand:PCRELOPT_VECT 1 "gpc_reg_operand" "wa")
		     (match_operand 2 "const_int_operand" "n")]
		    UNSPEC_PCREL_OPT_ST_RELOC))
   (clobber (match_operand:DI 3 "base_reg_operand" "=b"))]
  "TARGET_PCREL_OPT"
{
  output_pcrel_opt_reloc (operands[2]);
  return "stxv %x1,%0";
}
  [(set_attr "type" "vecstore")])
