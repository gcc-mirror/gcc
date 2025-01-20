;; Scheduling description for XiangShan Nanhu.

;; Nanhu is a 6-issue, superscalar, out-of-order processor.

;; -----------------------------------------------------
;; Nanhu Core units
;; 1*jmp + 4*alu + 2*mdu + 4*fma + 2*fmisc + 2*ld + 2*st
;; -----------------------------------------------------

(define_automaton "xiangshan")

(define_cpu_unit "xs_jmp" "xiangshan")
(define_cpu_unit "xs_i2f" "xiangshan")
(define_reservation "xs_jmp_rs" "xs_jmp | xs_i2f")

(define_cpu_unit "xs_alu_0, xs_alu_1, xs_alu_2, xs_alu_3" "xiangshan")
(define_reservation "xs_alu_rs"
  "xs_alu_0 | xs_alu_1 | xs_alu_2 | xs_alu_3")

(define_cpu_unit "xs_mul_0, xs_mul_1" "xiangshan")
(define_cpu_unit "xs_div_0, xs_div_1" "xiangshan")
(define_reservation "xs_mdu_rs"
  "(xs_mul_0 + xs_div_0) | (xs_mul_1 + xs_div_1)")

(define_cpu_unit "xs_fadd_0, xs_fadd_1, xs_fadd_2, xs_fadd_3" "xiangshan")
(define_cpu_unit "xs_fmul_0, xs_fmul_1, xs_fmul_2, xs_fmul_3" "xiangshan")
(define_reservation "xs_fma_0" "xs_fadd_0 + xs_fmul_0")
(define_reservation "xs_fma_1" "xs_fadd_1 + xs_fmul_1")
(define_reservation "xs_fma_2" "xs_fadd_2 + xs_fmul_2")
(define_reservation "xs_fma_3" "xs_fadd_3 + xs_fmul_3")

(define_cpu_unit "xs_f2f_0, xs_f2f_1" "xiangshan")
(define_cpu_unit "xs_f2i_0, xs_f2i_1" "xiangshan")
(define_cpu_unit "xs_fdiv_0, xs_fdiv_1" "xiangshan")
(define_reservation "xs_fmisc_rs"
  "(xs_f2f_0 + xs_f2i_0 + xs_fdiv_0) | (xs_f2f_1 + xs_f2i_1 + xs_fdiv_1)")

(define_cpu_unit "xs_ld_0, xs_ld_1" "xiangshan")
(define_cpu_unit "xs_st_0, xs_st_1" "xiangshan")
(define_reservation "xs_ld_rs" "xs_ld_0 | xs_ld_1")
(define_reservation "xs_st_rs" "xs_st_0 | xs_st_1")

;; ----------------------------------------------------
;; Memory (load/store)
;; ----------------------------------------------------

(define_insn_reservation "xiangshan_load" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "load"))
  "xs_ld_rs")

(define_insn_reservation "xiangshan_fpload" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fpload"))
  "xs_ld_rs")

(define_insn_reservation "xiangshan_store" 1
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "store"))
  "xs_st_rs")

(define_insn_reservation "xiangshan_fpstore" 1
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fpstore"))
  "xs_st_rs")

;; ----------------------------------------------------
;; Int
;; ----------------------------------------------------

(define_insn_reservation "xiangshan_jump" 1
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "jump,call,auipc,unknown,branch,jalr,ret,sfb_alu,trap"))
  "xs_jmp_rs")

(define_insn_reservation "xiangshan_i2f" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "mtc,fcvt_i2f"))
  "xs_jmp_rs")

(define_insn_reservation "xiangshan_atomic" 1
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "atomic"))
  "xs_jmp_rs")

(define_insn_reservation "xiangshan_mul" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "imul"))
  "xs_mdu_rs")

(define_insn_reservation "xiangshan_div" 21
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "idiv"))
  "xs_mdu_rs")

(define_insn_reservation "xiangshan_alu" 1
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "nop,const,branch,arith,shift,slt,multi,logical,move,bitmanip,unknown"))
  "xs_alu_rs")

;; Someone familiar with the xiangshan uarch needs to put
;; these into the right reservations.  This is just a placeholder
;; for everything I found that had no mapping to a reservation.
;;
;; Note that even if the processor does not implementat a particular
;; instruction it should still have suitable reservations, even if
;; they are just dummies like this one.
(define_insn_reservation "xiangshan_alu_unknown" 1
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "zicond,min,max,minu,maxu,clz,ctz,cpop,ghost,rotate,clmul,condmove,crypto,mvpair,rdvlenb,rdvl,wrvxrm,wrfrm,rdfrm,vsetvl,vsetvl_pre,vlde,vste,vldm,vstm,vlds,vsts,vldux,vldox,vstux,vstox,vldff,vldr,vstr,vlsegde,vssegte,vlsegds,vssegts,vlsegdux,vlsegdox,vssegtux,vssegtox,vlsegdff,vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,viminmax,vimul,vidiv,viwmul,vimuladd,sf_vqmacc,viwmuladd,vimerge,vimov,vsalu,vaalu,vsmul,vsshift,vnclip,sf_vfnrclip,vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,vired,viwred,vfredu,vfredo,vfwredu,vfwredo,vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,vfmovvf,vfmovfv,vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,vgather,vcompress,vmov,vector,vandn,vbrev,vbrev8,vrev8,vclz,vctz,vcpop,vrol,vror,vwsll,vclmul,vclmulh,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,vaeskf1,vaeskf2,vaesz,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,vsm3me,vsm3c,vfncvtbf16,vfwcvtbf16,vfwmaccbf16"))
  "xs_alu_rs")

;; ----------------------------------------------------
;; Float
;; ----------------------------------------------------


(define_insn_reservation "xiangshan_fma" 5
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fmadd"))
  "xs_fma_0 | xs_fma_1 | xs_fma_2 | xs_fma_3")

(define_insn_reservation "xiangshan_fadd" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fadd"))
  "xs_fadd_0 | xs_fadd_1 | xs_fadd_2 | xs_fadd_3")

(define_insn_reservation "xiangshan_fmul" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fmul"))
  "xs_fmul_0 | xs_fmul_1 | xs_fmul_2 | xs_fmul_3")

(define_insn_reservation "xiangshan_f2f" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fcvt,fcvt_f2i,fmove"))
  "xs_fmisc_rs")

(define_insn_reservation "xiangshan_f2i" 3
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "mfc,fcmp"))
  "xs_fmisc_rs")

(define_insn_reservation "xiangshan_sfdiv" 11
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fdiv")
       (eq_attr "mode" "SF"))
  "xs_fmisc_rs")

(define_insn_reservation "xiangshan_sfsqrt" 17
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fsqrt")
       (eq_attr "mode" "SF"))
  "xs_fmisc_rs")

(define_insn_reservation "xiangshan_dfdiv" 21
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fdiv")
       (eq_attr "mode" "DF"))
  "xs_fmisc_rs")

(define_insn_reservation "xiangshan_dfsqrt" 37
  (and (eq_attr "tune" "xiangshan")
       (eq_attr "type" "fsqrt")
       (eq_attr "mode" "DF"))
  "xs_fmisc_rs")
