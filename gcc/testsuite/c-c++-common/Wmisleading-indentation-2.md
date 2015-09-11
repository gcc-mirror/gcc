;; Support file for testcase Wmisleading-indentation.c
;; Adapted from gcc/config/i386/i386.md
(define_attr "cpu" "none,pentium,pentiumpro,geode,k6,athlon,k8,core2,nehalem,
		    atom,slm,generic,amdfam10,bdver1,bdver2,bdver3,bdver4,
		    btver2,knl"
  (const (symbol_ref "ix86_schedule")))

;; A basic instruction type.  Refinements due to arguments to be
;; provided in other attributes.
(define_attr "type"
  "other,multi,
   alu,alu1,negnot,imov,imovx,lea,
   incdec,ishift,ishiftx,ishift1,rotate,rotatex,rotate1,
   imul,imulx,idiv,icmp,test,ibr,setcc,icmov,
   push,pop,call,callv,leave,
   str,bitmanip,
   fmov,fop,fsgn,fmul,fdiv,fpspc,fcmov,fcmp,
   fxch,fistp,fisttp,frndint,
   sse,ssemov,sseadd,sseadd1,sseiadd,sseiadd1,
   ssemul,sseimul,ssediv,sselog,sselog1,
   sseishft,sseishft1,ssecmp,ssecomi,
   ssecvt,ssecvt1,sseicvt,sseins,
   sseshuf,sseshuf1,ssemuladd,sse4arg,
   lwp,mskmov,msklog,
   mmx,mmxmov,mmxadd,mmxmul,mmxcmp,mmxcvt,mmxshft,
   mpxmov,mpxmk,mpxchk,mpxld,mpxst"
  (const_string "other"))

;; Main data type used by the insn
(define_attr "mode"
  "unknown,none,QI,HI,SI,DI,TI,OI,XI,SF,DF,XF,TF,V16SF,V8SF,V4DF,V4SF,
  V2DF,V2SF,V1DF,V8DF"
  (const_string "unknown"))

;; The CPU unit operations uses.
(define_attr "unit" "integer,i387,sse,mmx,unknown"
  (cond [(eq_attr "type" "fmov,fop,fsgn,fmul,fdiv,fpspc,fcmov,fcmp,
			  fxch,fistp,fisttp,frndint")
	   (const_string "i387")
	 (eq_attr "type" "sse,ssemov,sseadd,sseadd1,sseiadd,sseiadd1,
			  ssemul,sseimul,ssediv,sselog,sselog1,
			  sseishft,sseishft1,ssecmp,ssecomi,
			  ssecvt,ssecvt1,sseicvt,sseins,
			  sseshuf,sseshuf1,ssemuladd,sse4arg,mskmov")
	   (const_string "sse")
	 (const_string "integer")))
