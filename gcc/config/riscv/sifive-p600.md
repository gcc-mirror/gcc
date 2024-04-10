;; Scheduling description for Sifive p600 series.

;; Sifive p600 series is a quad-issue, superscalar, out-of-order processor.

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
(define_automaton "sifive_p600_iex,sifive_p600_fex,sifive_p600_mem,sifive_p600_div")

;; The Sifive p600 has 7 pipelines:
;; A-pipe       Load, Store
;; B1-pipe      ALU, Branch
;; B2-pipe      ALU, Branch
;; M-pipe       ALU, MUL, DIV and I2F(integer to float instruction)
;; C-pipe       ALU, Conditional move and system for coprocessor accesses
;; F-pipe       FPU, MUL, F2I(float to integer instruction)
;; FM-pipe      FPU, MUL, DIV

(define_cpu_unit "sifive_p600_A" "sifive_p600_mem")
(define_cpu_unit "sifive_p600_B1" "sifive_p600_iex")
(define_cpu_unit "sifive_p600_B2" "sifive_p600_iex")
(define_cpu_unit "sifive_p600_M" "sifive_p600_iex")
(define_cpu_unit "sifive_p600_C" "sifive_p600_iex")
(define_cpu_unit "sifive_p600_F" "sifive_p600_fex")
(define_cpu_unit "sifive_p600_FM" "sifive_p600_fex")

;; Load and store unit.
(define_cpu_unit "sifive_p600_ld" "sifive_p600_mem")
(define_cpu_unit "sifive_p600_st" "sifive_p600_mem")

;; Branch unit.
(define_cpu_unit "sifive_p600_bru" "sifive_p600_iex")

;; Integer and multiply unit.
(define_cpu_unit "sifive_p600_ialu" "sifive_p600_iex")
(define_cpu_unit "sifive_p600_imul" "sifive_p600_iex")
(define_cpu_unit "sifive_p600_system" "sifive_p600_iex")

;; Divide unit.
(define_cpu_unit "sifive_p600_idiv" "sifive_p600_div")
(define_cpu_unit "sifive_p600_fdiv" "sifive_p600_div")

;; Float and multiply unit.
(define_cpu_unit "sifive_p600_fmul" "sifive_p600_fex")
(define_cpu_unit "sifive_p600_fpu" "sifive_p600_fex")

;; ALU instruction can use pipeline C, B1, B2 and M.
(define_reservation "int_pipe" "(sifive_p600_C|sifive_p600_B1|sifive_p600_B2|sifive_p600_M)")
;; FPU instruction can use pipeline F and FM.
(define_reservation "float_pipe" "(sifive_p600_F|sifive_p600_FM)")
;; Branch instruction can use pipeline B1 and B2.
(define_reservation "branch_pipe" "(sifive_p600_B1|sifive_p600_B2)")

(define_insn_reservation "sifive_p600_load" 3
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "load"))
  "sifive_p600_A,sifive_p600_ld*2")

(define_insn_reservation "sifive_p600_fpload" 4
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "fpload"))
  "sifive_p600_A,sifive_p600_ld*3")

(define_insn_reservation "sifive_p600_store" 1
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "store"))
  "sifive_p600_A+sifive_p600_st")

(define_insn_reservation "sifive_p600_fpstore" 1
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "fpstore"))
  "sifive_p600_A+sifive_p600_st")

(define_insn_reservation "sifive_p600_branch" 1
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap"))
  "branch_pipe+sifive_p600_bru")

(define_insn_reservation "sifive_p600_sfb_alu" 1
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "sfb_alu"))
  "sifive_p600_C+sifive_p600_bru+sifive_p600_ialu")

(define_insn_reservation "sifive_p600_atomic" 3
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "atomic"))
  "sifive_p600_C,sifive_p600_system*2")

(define_insn_reservation "sifive_p600_mul" 3
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "imul"))
  "sifive_p600_M,sifive_p600_imul*2")

(define_insn_reservation "sifive_p600_div" 16
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "idiv"))
  "sifive_p600_M, sifive_p600_idiv*4")

(define_insn_reservation "sifive_p600_alu" 1
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "unknown,arith,logical,shift,slt,multi,bitmanip,\
			clz,ctz,rotate,min,max,minu,maxu,condmove,mvpair,zicond"))
  "int_pipe+sifive_p600_ialu")

(define_insn_reservation "sifive_p600_cpop" 3
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "cpop"))
  "int_pipe,sifive_p600_ialu*2")

(define_insn_reservation "sifive_p600_clmul" 3
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "clmul"))
  "int_pipe,sifive_p600_ialu*2")

(define_insn_reservation "sifive_p600_load_immediate" 1
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "nop,const,auipc,move"))
  "int_pipe")

(define_insn_reservation "sifive_p600_fma" 4
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "fadd,fmul,fmadd"))
  "float_pipe,sifive_p600_fmul*3")

(define_insn_reservation "sifive_p600_i2f" 2
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "mtc,fcvt_i2f"))
  "sifive_p600_M,sifive_p600_ialu")

(define_insn_reservation "sifive_p600_f2i" 2
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "mfc,fcmp,fcvt_f2i"))
  "sifive_p600_F,sifive_p600_fpu")

(define_insn_reservation "sifive_p600_fmove" 2
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "fmove,fcvt"))
  "float_pipe,sifive_p600_fpu")

(define_insn_reservation "sifive_p600_fdiv_s" 11
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "SF"))
  "sifive_p600_FM, sifive_p600_fdiv*5")

(define_insn_reservation "sifive_p600_fdiv_d" 19
  (and (eq_attr "tune" "sifive_p600")
       (eq_attr "type" "fdiv,fsqrt")
       (eq_attr "mode" "DF"))
  "sifive_p600_FM, sifive_p600_fdiv*5")

(define_bypass 1 "sifive_p600_load,sifive_p600_alu,sifive_p600_mul,sifive_p600_sfb_alu"
  "sifive_p600_alu,sifive_p600_branch")

(define_bypass 1 "sifive_p600_load,sifive_p600_alu,sifive_p600_mul,
                  sifive_p600_f2i,sifive_p600_fmove,sifive_p600_sfb_alu"
  "sifive_p600_store" "riscv_store_data_bypass_p")

(define_bypass 1 "sifive_p600_i2f"
  "sifive_p600_fma,sifive_p600_f2i,sifive_p600_fmove,sifive_p600_fdiv_s,sifive_p600_fdiv_d")

(define_bypass 1 "sifive_p600_f2i"
  "sifive_p600_branch,sifive_p600_sfb_alu,sifive_p600_mul,
   sifive_p600_div,sifive_p600_alu,sifive_p600_cpop")
