(define_automaton "sifive_7,sifive_7_vec,sifive_7_vec_mem")

;; Sifive 7 Series Base Core
;; This has two pipelines, A (Address) and B (Branch).
;; Loads, stores, and FP <-> integer moves use the A-pipe.
;; Branches, MUL/DIV, and FP ops use the B-pipe.
;; Integer ALU ops can use either pipe.

(define_cpu_unit "sifive_7_A" "sifive_7")
(define_cpu_unit "sifive_7_B" "sifive_7")

(define_cpu_unit "sifive_7_idiv" "sifive_7")
(define_cpu_unit "sifive_7_fpu" "sifive_7")
;; Vector command queue
(define_cpu_unit "sifive_7_vcq" "sifive_7")
;; Vector arithmetic sequencer
(define_cpu_unit "sifive_7_va" "sifive_7_vec")
;; Vector store sequencer
(define_cpu_unit "sifive_7_vs" "sifive_7_vec_mem")
;; Vector load sequencer
(define_cpu_unit "sifive_7_vl" "sifive_7_vec_mem")

(define_insn_reservation "sifive_7_load" 3
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "load"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_fpload" 2
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "fpload"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_store" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "store"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_fpstore" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "fpstore"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_branch" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "branch,ret,trap"))
  "sifive_7_B")

(define_insn_reservation "sifive_7_sfb_alu" 2
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "sfb_alu"))
  "sifive_7_A+sifive_7_B")

(define_insn_reservation "sifive_7_jump" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "jump,call,jalr"))
  "sifive_7_B")

(define_insn_reservation "sifive_7_mul" 3
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "imul"))
  "sifive_7_B")

(define_insn_reservation "sifive_7_div" 16
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "idiv"))
  "sifive_7_B,sifive_7_idiv*15")

(define_insn_reservation "sifive_7_alu" 2
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "unknown,arith,shift,slt,multi,logical,move,bitmanip,\
			min,max,minu,maxu,atomic,condmove,mvpair,zicond"))
  "sifive_7_A|sifive_7_B")

(define_insn_reservation "sifive_7_alu_b" 2
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "clz,ctz,rotate"))
  "sifive_7_B")

(define_insn_reservation "sifive_7_load_immediate" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "nop,const,auipc"))
  "sifive_7_A|sifive_7_B")

(define_insn_reservation "sifive_7_hfma" 5
  (and (eq_attr "tune" "sifive_7")
       (and (eq_attr "type" "fadd,fmul,fmadd")
	    (eq_attr "mode" "HF")))
  "sifive_7_B")

(define_insn_reservation "sifive_7_sfma" 5
  (and (eq_attr "tune" "sifive_7")
       (and (eq_attr "type" "fadd,fmul,fmadd")
	    (eq_attr "mode" "SF")))
  "sifive_7_B")

(define_insn_reservation "sifive_7_dfma" 7
  (and (eq_attr "tune" "sifive_7")
       (and (eq_attr "type" "fadd,fmul,fmadd")
	    (eq_attr "mode" "DF")))
  "sifive_7_B")

(define_insn_reservation "sifive_7_fp_other" 3
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "fcvt,fcvt_i2f,fcvt_f2i,fcmp,fmove"))
  "sifive_7_B")

(define_insn_reservation "sifive_7_fdiv_h" 14
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "HF"))
  "sifive_7_B,sifive_7_fpu*13")

(define_insn_reservation "sifive_7_fdiv_s" 27
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "SF"))
  "sifive_7_B,sifive_7_fpu*26")

(define_insn_reservation "sifive_7_fdiv_d" 56
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "DF"))
  "sifive_7_B,sifive_7_fpu*55")

(define_insn_reservation "sifive_7_i2f" 3
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "mtc"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_f2i" 3
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "mfc"))
  "sifive_7_A")

;; Popcount and clmul.
(define_insn_reservation "sifive_7_popcount" 2
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "cpop,clmul"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_csr" 5
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "rdfrm,wrfrm,wrvxrm"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_crypto" 10
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "crypto"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_unknown" 10
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "ghost"))
  "sifive_7_A")

(define_bypass 1 "sifive_7_load,sifive_7_alu,sifive_7_mul,sifive_7_f2i,sifive_7_sfb_alu"
  "sifive_7_alu,sifive_7_branch")

(define_bypass 1 "sifive_7_alu,sifive_7_sfb_alu"
  "sifive_7_sfb_alu")

(define_bypass 1 "sifive_7_load,sifive_7_alu,sifive_7_mul,sifive_7_f2i,sifive_7_sfb_alu"
  "sifive_7_store" "riscv_store_data_bypass_p")

(define_bypass 2 "sifive_7_i2f"
  "sifive_7_sfma,sifive_7_dfma,sifive_7_fp_other,sifive_7_fdiv_h,sifive_7_fdiv_s,sifive_7_fdiv_d,sifive_7_hfma")

(define_bypass 2 "sifive_7_fp_other"
  "sifive_7_sfma,sifive_7_dfma,sifive_7_fp_other,sifive_7_fdiv_h,sifive_7_fdiv_s,sifive_7_fdiv_d,sifive_7_hfma")

(define_bypass 2 "sifive_7_fp_other"
  "sifive_7_alu,sifive_7_branch")

(define_bypass 2 "sifive_7_fp_other"
  "sifive_7_store" "riscv_store_data_bypass_p")

;; Vector pipeline
;; The latency is depend on LMUL, but we didn't model that yet since we don't
;; want to expand the rule too much unless we prove model that could get
;; meaningful performance difference.

(define_insn_reservation "sifive_7_vsetvl" 2
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vsetvl_pre,vsetvl,rdvlenb,rdvl"))
  "sifive_7_A")

(define_insn_reservation "sifive_7_vec_load" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vlde,vldm,vlds,vldux,vldox,vldff,vldr,
                        vlsegde,vlsegds,vlsegdux,vlsegdox,vlsegdff"))
  "sifive_7_vcq,sifive_7_vl*3")

(define_insn_reservation "sifive_7_vec_store" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vste,vstm,vsts,vstux,vstox,vstr,
                        vssegte,vssegts,vssegtux,vssegtox"))
  "sifive_7_vcq,sifive_7_vs*3")

(define_insn_reservation "sifive_7_vec_ialu" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vimovxv,vmov,vimovvx,vialu,vicalu,vext,
                        vshift,viminmax,vimerge,vbrev,vrev8,
                        vimov,vext,vbrev8,vclz,vctz,vcpop,vrol,vror,vandn"))
  "sifive_7_vcq,sifive_7_va*3")

(define_insn_reservation "sifive_7_vec_slow_ialu" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vshift,vimul,vimuladd"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_cmp" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vicmp"))
  "sifive_7_vcq,sifive_7_va*3")

(define_insn_reservation "sifive_7_vec_iwalu" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "viwalu,viwmul,viwmuladd,vnshift,vwsll"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_div" 16
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vidiv,vfdiv"))
  "sifive_7_vcq,sifive_7_va*15")

(define_insn_reservation "sifive_7_vec_fixed_point" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vsalu,vaalu,vsmul,vsshift"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_narrow_fixed_point" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vnclip"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_fsimple" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vfmovvf,vfmovfv,vfclass"))
  "sifive_7_vcq,sifive_7_va*3")

(define_insn_reservation "sifive_7_vec_falu" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vfalu,vfmul,vfmuladd,vfrecp,
                        vfcvtitof,vfcvtftoi,vfmerge,vfmov,vfsgnj"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_fcmp" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vfcmp,vfminmax"))
  "sifive_7_vcq,sifive_7_va*3")

(define_insn_reservation "sifive_7_vec_fsqrt_fdiv" 16
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vfsqrt,vfdiv"))
  "sifive_7_vcq,sifive_7_va*15")

(define_insn_reservation "sifive_7_vec_fwalu" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vfwalu,vfwmul,vfwmuladd,vfwmaccbf16,vfwcvtitof,
                        vfwcvtftoi,vfwcvtftof,vfwcvtbf16,
                        vfncvtitof,vfncvtftoi,vfncvtftof,vfncvtbf16,
                        sf_vfnrclip,sf_vqmacc"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_red" 12
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vired,vfredu,vfredo,viwred,vfwredu,vfwredo"))
  "sifive_7_vcq,sifive_7_va*11")

(define_insn_reservation "sifive_7_vec_mask" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vmalu,vmpop,vmffs,vmsfs"))
  "sifive_7_vcq,sifive_7_va*3")

(define_insn_reservation "sifive_7_vec_mask_special" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vmiota,vmidx"))
  "sifive_7_vcq,sifive_7_va*3")

(define_insn_reservation "sifive_7_vec_gather" 8
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vgather"))
  "sifive_7_vcq,sifive_7_va*7")

(define_insn_reservation "sifive_7_vec_compress" 16
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vcompress"))
  "sifive_7_vcq,sifive_7_va*15")

(define_insn_reservation "sifive_7_vec_slide" 4
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down"))
  "sifive_7_vcq,sifive_7_va*3")

;; Assume that's slow if it's unknown instruction vector type.
(define_insn_reservation "sifive_7_vec_unknown" 16
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "vector,vclmul,vclmulh,vghsh,vgmul,
                        vaesef,vaesem,vaesdf,vaesdm,vaeskf1,vaeskf2,
                        vaesz,vsha2ms,vsha2ch,vsha2cl,
                        vsm4k,vsm4r,vsm3me,vsm3c,sf_vc,sf_vc_se"))
  "sifive_7_vcq,sifive_7_va*15")
