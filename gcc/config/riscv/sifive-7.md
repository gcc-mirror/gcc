(define_automaton "sifive_7")

;; Sifive 7 Series Base Core
;; This has two pipelines, A (Address) and B (Branch).
;; Loads, stores, and FP <-> integer moves use the A-pipe.
;; Branches, MUL/DIV, and FP ops use the B-pipe.
;; Integer ALU ops can use either pipe.

(define_cpu_unit "sifive_7_A" "sifive_7")
(define_cpu_unit "sifive_7_B" "sifive_7")

(define_cpu_unit "sifive_7_idiv" "sifive_7")
(define_cpu_unit "sifive_7_fpu" "sifive_7")

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
       (eq_attr "type" "branch"))
  "sifive_7_B")

(define_insn_reservation "sifive_7_jump" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "jump,call"))
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
       (eq_attr "type" "unknown,arith,shift,slt,multi,logical,move"))
  "sifive_7_A|sifive_7_B")

(define_insn_reservation "sifive_7_load_immediate" 1
  (and (eq_attr "tune" "sifive_7")
       (eq_attr "type" "nop,const,auipc"))
  "sifive_7_A|sifive_7_B")

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
       (eq_attr "type" "fcvt,fcmp,fmove"))
  "sifive_7_B")

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

(define_bypass 1 "sifive_7_load,sifive_7_alu,sifive_7_mul,sifive_7_f2i"
  "sifive_7_alu,sifive_7_branch")

(define_bypass 1 "sifive_7_load,sifive_7_alu,sifive_7_mul,sifive_7_f2i"
  "sifive_7_store" "riscv_store_data_bypass_p")

(define_bypass 2 "sifive_7_i2f"
  "sifive_7_sfma,sifive_7_dfma,sifive_7_fp_other,sifive_7_fdiv_s,sifive_7_fdiv_d")

(define_bypass 2 "sifive_7_fp_other"
  "sifive_7_sfma,sifive_7_dfma,sifive_7_fp_other,sifive_7_fdiv_s,sifive_7_fdiv_d")

(define_bypass 2 "sifive_7_fp_other"
  "sifive_7_alu,sifive_7_branch")

(define_bypass 2 "sifive_7_fp_other"
  "sifive_7_store" "riscv_store_data_bypass_p")
