(define_automaton "ppc7xx,ppc7xxfp,ppc7xxother,ppc7xxvec")
(define_cpu_unit "iu1_7xx,iu2_7xx" "ppc7xx")
(define_cpu_unit "fpu_7xx" "ppc7xxfp")
(define_cpu_unit "lsu_7xx,bpu_7xx,sru_7xx" "ppc7xxother")
(define_cpu_unit "du1_7xx,du2_7xx" "ppc7xx")
(define_cpu_unit "veccmplx_7xx,vecperm_7xx,vdu_7xx" "ppc7xxvec")

;; PPC740/PPC750/PPC7400  32-bit 2xIU, LSU, SRU, FPU, BPU
;; IU1 can perform all integer operations
;; IU2 can perform all integer operations except imul and idiv
;; LSU 2 stage pipelined
;; FPU 3 stage pipelined
;; Max issue 3 insns/clock cycle (includes 1 branch)
;; In-order execution


;; The PPC750 user's manual recommends that to reduce branch mispredictions,
;; the insn that sets CR bits should be separated from the branch insn
;; that evaluates them.  There is no advantage have more than 10 cycles
;; of separation.
;; This could be artificially achieved by exagerating the latency of
;; compare insns but at the expense of a poorer schedule.

;; Branches go straight to the BPU.  All other insns are handled
;; by a dispatch unit which can issue a max of 2 insns per cycle.
(define_reservation "ppc750_du" "du1_7xx|du2_7xx")
(define_reservation "ppc7400_vec_du" "vdu_7xx")

(define_insn_reservation "ppc750-load" 2
  (and (eq_attr "type" "load,load_ext,load_ext_u,load_ext_ux,\
		        load_ux,load_u,fpload,fpload_ux,fpload_u,vecload")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,lsu_7xx")

(define_insn_reservation "ppc750-store" 1
  (and (eq_attr "type" "store,store_ux,store_u,\
		        fpstore,fpstore_ux,fpstore_u,vecstore")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,lsu_7xx")

(define_insn_reservation "ppc750-integer" 1
  (and (eq_attr "type" "integer")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,(iu1_7xx|iu2_7xx)")

(define_insn_reservation "ppc750-imul" 4
  (and (eq_attr "type" "imul")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,iu1_7xx*4")

(define_insn_reservation "ppc750-imul2" 3
  (and (eq_attr "type" "imul2")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,iu1_7xx*2")

(define_insn_reservation "ppc750-imul3" 2
  (and (eq_attr "type" "imul3")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,iu1_7xx")

(define_insn_reservation "ppc750-idiv" 19
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,iu1_7xx*19")

(define_insn_reservation "ppc750-compare" 2
  (and (eq_attr "type" "cmp,compare,delayed_compare")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,(iu1_7xx|iu2_7xx)")

(define_insn_reservation "ppc750-fpcompare" 2
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,fpu_7xx")

(define_insn_reservation "ppc750-fp" 3
  (and (eq_attr "type" "fp")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,fpu_7xx")

(define_insn_reservation "ppc750-dmul" 4
  (and (eq_attr "type" "dmul")
       (eq_attr "cpu" "ppc750"))
  "ppc750_du,fpu_7xx*2")

(define_insn_reservation "ppc7400-dmul" 3
  (and (eq_attr "type" "dmul")
       (eq_attr "cpu" "ppc7400"))
  "ppc750_du,fpu_7xx")

; Divides are not pipelined
(define_insn_reservation "ppc750-sdiv" 17
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,fpu_7xx*17")

(define_insn_reservation "ppc750-ddiv" 31
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,fpu_7xx*31")

(define_insn_reservation "ppc750-mfcr" 2
  (and (eq_attr "type" "mfcr,mtcr")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,iu1_7xx")

(define_insn_reservation "ppc750-crlogical" 3
  (and (eq_attr "type" "cr_logical,delayed_cr")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "ppc750_du,sru_7xx*2")

(define_insn_reservation "ppc750-mtjmpr" 2
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "nothing,sru_7xx*2")

(define_insn_reservation "ppc750-jmpreg" 1
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "ppc750,ppc7400"))
  "nothing,bpu_7xx")

;; Altivec
(define_insn_reservation "ppc7400-vecsimple" 1
  (and (eq_attr "type" "vecsimple,veccmp")
       (eq_attr "cpu" "ppc7400"))
  "ppc750_du,ppc7400_vec_du,veccmplx_7xx")

(define_insn_reservation "ppc7400-veccomplex" 4
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "ppc7400"))
  "ppc750_du,ppc7400_vec_du,veccmplx_7xx")

(define_insn_reservation "ppc7400-vecfloat" 4
  (and (eq_attr "type" "vecfloat")
       (eq_attr "cpu" "ppc7400"))
  "ppc750_du,ppc7400_vec_du,veccmplx_7xx")

(define_insn_reservation "ppc7400-vecperm" 2
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "ppc7400"))
  "ppc750_du,ppc7400_vec_du,vecperm_7xx")

