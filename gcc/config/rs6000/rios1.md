(define_automaton "rios1,rios1fp")
(define_cpu_unit "iu_rios1" "rios1")
(define_cpu_unit "fpu_rios1" "rios1fp")
(define_cpu_unit "bpu_rios1" "rios1")

;; RIOS1  32-bit IU, FPU, BPU

(define_insn_reservation "rios1-load" 2
  (and (eq_attr "type" "load,load_ext,load_ext_u,load_ext_ux,load_ux,load_u")
       (eq_attr "cpu" "rios1,ppc601"))
  "iu_rios1")

(define_insn_reservation "rios1-store" 1
  (and (eq_attr "type" "store,store_ux,store_u")
       (eq_attr "cpu" "rios1,ppc601"))
  "iu_rios1")

(define_insn_reservation "rios1-fpload" 2
  (and (eq_attr "type" "fpload,fpload_ux,fpload_u")
       (eq_attr "cpu" "rios1"))
  "iu_rios1")

(define_insn_reservation "ppc601-fpload" 3
  (and (eq_attr "type" "fpload,fpload_ux,fpload_u")
       (eq_attr "cpu" "ppc601"))
  "iu_rios1")

(define_insn_reservation "rios1-fpstore" 1
  (and (eq_attr "type" "fpstore,fpstore_ux,fpstore_u")
       (eq_attr "cpu" "rios1,ppc601"))
  "iu_rios1+fpu_rios1")

(define_insn_reservation "rios1-integer" 1
  (and (eq_attr "type" "integer,mfcr,mtcr")
       (eq_attr "cpu" "rios1,ppc601"))
  "iu_rios1")

(define_insn_reservation "rios1-imul" 5
  (and (eq_attr "type" "imul")
       (eq_attr "cpu" "rios1"))
  "iu_rios1*5")

(define_insn_reservation "rios1-imul2" 4
  (and (eq_attr "type" "imul2")
       (eq_attr "cpu" "rios1"))
  "iu_rios1*4")

(define_insn_reservation "rios1-imul3" 3
  (and (eq_attr "type" "imul")
       (eq_attr "cpu" "rios1"))
  "iu_rios1*3")

(define_insn_reservation "ppc601-imul" 5
  (and (eq_attr "type" "imul,imul2,imul3")
       (eq_attr "cpu" "ppc601"))
  "iu_rios1*5")

(define_insn_reservation "rios1-idiv" 19
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "rios1"))
  "iu_rios1*19")

(define_insn_reservation "ppc601-idiv" 36
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "ppc601"))
  "iu_rios1*36")

; compare executes on integer unit, but feeds insns which
; execute on the branch unit.
(define_insn_reservation "rios1-compare" 4
  (and (eq_attr "type" "cmp,compare")
       (eq_attr "cpu" "rios1"))
  "iu_rios1,nothing*2,bpu_rios1")

(define_insn_reservation "rios1-delayed_compare" 5
  (and (eq_attr "type" "delayed_compare")
       (eq_attr "cpu" "rios1"))
  "iu_rios1,nothing*3,bpu_rios1")

(define_insn_reservation "ppc601-compare" 3
  (and (eq_attr "type" "cmp,compare,delayed_compare")
       (eq_attr "cpu" "ppc601"))
  "iu_rios1,nothing,bpu_rios1")

(define_insn_reservation "rios1-fpcompare" 9
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "rios1"))
  "fpu_rios1,nothing*3,bpu_rios1")

(define_insn_reservation "ppc601-fpcompare" 5
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "ppc601"))
  "(fpu_rios1+iu_rios1*2),nothing*2,bpu_rios1")

(define_insn_reservation "rios1-fp" 2
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "rios1"))
  "fpu_rios1")

(define_insn_reservation "ppc601-fp" 4
  (and (eq_attr "type" "fp")
       (eq_attr "cpu" "ppc601"))
  "fpu_rios1")

(define_insn_reservation "rios1-dmul" 5
  (and (eq_attr "type" "dmul")
       (eq_attr "cpu" "ppc601"))
  "fpu_rios1*2")

(define_insn_reservation "rios1-sdiv" 19
  (and (eq_attr "type" "sdiv,ddiv")
       (eq_attr "cpu" "rios1"))
  "fpu_rios1*19")

(define_insn_reservation "ppc601-sdiv" 17
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "ppc601"))
  "fpu_rios1*17")

(define_insn_reservation "ppc601-ddiv" 31
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "ppc601"))
  "fpu_rios1*31")

(define_insn_reservation "rios1-crlogical" 4
  (and (eq_attr "type" "cr_logical,delayed_cr")
       (eq_attr "cpu" "rios1,ppc601"))
  "bpu_rios1")

(define_insn_reservation "rios1-mtjmpr" 5
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "rios1"))
  "bpu_rios1")

(define_insn_reservation "ppc601-mtjmpr" 4
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "ppc601"))
  "bpu_rios1")

(define_insn_reservation "rios1-branch" 1
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "rios1,ppc601"))
  "bpu_rios1")

