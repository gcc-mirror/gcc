;; Scheduling description for Sifive p400.

;; Sifive p400 series is a triple-issue, superscalar, out-of-order processor.

;; CPU execution units:
;; ialu            Integer Units: all arithmetic and logic.
;;
;; bru             Branch Resolution Unit: all branches.
;;
;; st              Memory Write Unit: all writes to memory.
;;
;; ld              Memory Read Unit: all reads from memory.
;;
;; imul            Integer Multiply Unit
;;
;; idiv            Integer Divide Unit
;;
;; system          System Unit: all coprocessor accesses.
;;
;; fpu             Floating Point Unit
;;
;; fmul            Floating Point Multiply Unit
;;
;; fdiv            Floating Point Divide Unit

;; Four automata are defined to reduce number of states
;; which a single large automaton will have.
(define_automaton "sifive_p400_iex,sifive_p400_fex,sifive_p400_mem,sifive_p400_div")

;; The Sifive p400 has six pipelines:
;; A-pipe       Load, Store
;; B-pipe       ALU, Branch
;; M-pipe       ALU, MUL, DIV and I2F(integer to float instruction)
;; C-pipe       ALU, Conditional move and system for coprocessor accesses
;; F-pipe       FPU, MUL, F2I(float to integer instruction)
;; FM-pipe      FPU, MUL, DIV

(define_cpu_unit "sifive_p400_A" "sifive_p400_mem")
(define_cpu_unit "sifive_p400_B" "sifive_p400_iex")
(define_cpu_unit "sifive_p400_M" "sifive_p400_iex")
(define_cpu_unit "sifive_p400_C" "sifive_p400_iex")
(define_cpu_unit "sifive_p400_F" "sifive_p400_fex")
(define_cpu_unit "sifive_p400_FM" "sifive_p400_fex")

;; Load and store unit.
(define_cpu_unit "sifive_p400_ld" "sifive_p400_mem")
(define_cpu_unit "sifive_p400_st" "sifive_p400_mem")

;; Branch unit.
(define_cpu_unit "sifive_p400_bru" "sifive_p400_iex")

;; Integer and multiply unit.
(define_cpu_unit "sifive_p400_ialu" "sifive_p400_iex")
(define_cpu_unit "sifive_p400_imul" "sifive_p400_iex")
(define_cpu_unit "sifive_p400_system" "sifive_p400_iex")

;; Divide unit.
(define_cpu_unit "sifive_p400_idiv" "sifive_p400_div")
(define_cpu_unit "sifive_p400_fdiv" "sifive_p400_div")

;; Float and multiply unit.
(define_cpu_unit "sifive_p400_fmul" "sifive_p400_fex")
(define_cpu_unit "sifive_p400_fpu" "sifive_p400_fex")

;; ALU instruction can use pipeline C, B and M.
(define_reservation "p400_int_pipe" "(sifive_p400_C|sifive_p400_B|sifive_p400_M)")
;; FPU instruction can use pipeline F and FM.
(define_reservation "p400_float_pipe" "(sifive_p400_F|sifive_p400_FM)")

(define_insn_reservation "sifive_p400_load" 3
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "load"))
  "sifive_p400_A,sifive_p400_ld*2")

(define_insn_reservation "sifive_p400_fpload" 4
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "fpload"))
  "sifive_p400_A,sifive_p400_ld*3")

(define_insn_reservation "sifive_p400_store" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "store"))
  "sifive_p400_A+sifive_p400_st")

(define_insn_reservation "sifive_p400_fpstore" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "fpstore"))
  "sifive_p400_A+sifive_p400_st")

(define_insn_reservation "sifive_p400_branch" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap"))
  "sifive_p400_B+sifive_p400_bru")

(define_insn_reservation "sifive_p400_sfb_alu" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "sfb_alu"))
  "sifive_p400_C+sifive_p400_bru+sifive_p400_ialu")

(define_insn_reservation "sifive_p400_atomic" 3
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "atomic"))
  "sifive_p400_C,sifive_p400_system*2")

(define_insn_reservation "sifive_p400_mul" 3
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "imul"))
  "sifive_p400_M,sifive_p400_imul*2")

(define_insn_reservation "sifive_p400_div" 31
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "idiv"))
  "sifive_p400_M, sifive_p400_idiv*5")

(define_insn_reservation "sifive_p400_alu" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "unknown,arith,logical,shift,slt,multi,bitmanip,\
			clz,ctz,rotate,min,max,minu,maxu,condmove,mvpair,zicond"))
  "p400_int_pipe+sifive_p400_ialu")

(define_insn_reservation "sifive_p400_cpop" 3
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "cpop"))
  "p400_int_pipe,sifive_p400_ialu*2")

(define_insn_reservation "sifive_p400_clmul" 3
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "clmul"))
  "p400_int_pipe,sifive_p400_ialu*2")

(define_insn_reservation "sifive_p400_load_immediate" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "nop,const,auipc,move"))
  "p400_int_pipe")

(define_insn_reservation "sifive_p400_fma" 4
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "fadd,fmul,fmadd"))
  "p400_float_pipe,sifive_p400_fmul*3")

(define_insn_reservation "sifive_p400_i2f" 2
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "mtc,fcvt_i2f"))
  "sifive_p400_M,sifive_p400_ialu")

(define_insn_reservation "sifive_p400_f2i" 2
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "mfc,fcmp,fcvt_f2i"))
  "sifive_p400_F,sifive_p400_fpu")

(define_insn_reservation "sifive_p400_fmove" 2
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "fmove,fcvt"))
  "p400_float_pipe,sifive_p400_fpu")

;; We need something for HF so that we don't abort during
;; scheduling if someone was to ask for p400 scheduling, but
;; enable the various HF mode extensions.
(define_insn_reservation "sifive_p400_fdiv_s" 18
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "HF,SF"))
  "sifive_p400_FM, sifive_p400_fdiv*5")

(define_insn_reservation "sifive_p400_fdiv_d" 31
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "DF"))
  "sifive_p400_FM, sifive_p400_fdiv*5")

(define_bypass 1 "sifive_p400_load,sifive_p400_alu,sifive_p400_mul,sifive_p400_sfb_alu"
  "sifive_p400_alu,sifive_p400_branch")

(define_bypass 1 "sifive_p400_load,sifive_p400_alu,sifive_p400_mul,
                  sifive_p400_f2i,sifive_p400_fmove,sifive_p400_sfb_alu"
  "sifive_p400_store" "riscv_store_data_bypass_p")

(define_bypass 1 "sifive_p400_i2f"
  "sifive_p400_fma,sifive_p400_f2i,sifive_p400_fmove,sifive_p400_fdiv_s,sifive_p400_fdiv_d")

(define_bypass 1 "sifive_p400_f2i"
  "sifive_p400_branch,sifive_p400_sfb_alu,sifive_p400_mul,
   sifive_p400_div,sifive_p400_alu,sifive_p400_cpop")


;; Someone familiar with the p400 uarch needs to put
;; these into the right reservations.  This is just a placeholder
;; for everything I found that had no mapping to a reservation.
;;
;; Note that even if the processor does not implementat a particular
;; instruction it should still have suitable reservations, even if
;; they are just dummies like this one.
(define_insn_reservation "sifive_p400_unknown" 1
  (and (eq_attr "tune" "sifive_p400")
       (eq_attr "type" "ghost,vfrecp,vclmul,vldm,vmffs,vclmulh,vlsegde,vfcvtitof,vsm4k,vfcvtftoi,vfdiv,vsm3c,vsm4r,viwmuladd,vfwredu,vcpop,vfwmuladd,vstux,vsshift,vfwcvtftof,vfncvtftof,vfwmaccbf16,vext,vssegte,rdvl,vaeskf1,vfslide1up,vmov,vimovvx,vaesef,vfsqrt,viminmax,vfwcvtftoi,vssegtox,vfclass,viwmul,vector,vgmul,vsm3me,vfcmp,vstm,vfredo,vfwmul,vaeskf2,vstox,vfncvtbf16,vislide1up,vgather,vldox,viwred,vctz,vghsh,vsts,vslidedown,vfmerge,vicmp,vsmul,vlsegdff,vfalu,vfmov,vislide1down,vfminmax,vcompress,vldr,vldff,vlsegdux,vimuladd,vsalu,vidiv,sf_vqmacc,vfslide1down,vaesem,vimerge,vfncvtftoi,vfwcvtitof,vicalu,vaesz,sf_vc_se,vsha2cl,vmsfs,vldux,vmidx,vslideup,vired,vlde,vfwredo,vfmovfv,vbrev,vfncvtitof,rdfrm,vsetvl,vssegts,vimul,vialu,vbrev8,vfwalu,rdvlenb,sf_vfnrclip,vclz,vnclip,sf_vc,vimov,vste,vfmuladd,vfmovvf,vwsll,vsetvl_pre,vlds,vlsegds,vmiota,vmalu,wrvxrm,wrfrm,viwalu,vaesdm,vssegtux,vaesdf,vimovxv,vror,vnshift,vstr,vaalu,vsha2ms,crypto,vfwcvtbf16,vlsegdox,vrol,vandn,vfsgnj,vmpop,vfredu,vsha2ch,vshift,vrev8,vfmul"))
  "p400_int_pipe+sifive_p400_ialu")


