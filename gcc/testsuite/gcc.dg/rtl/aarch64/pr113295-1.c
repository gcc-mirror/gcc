// { dg-do run { target aarch64*-*-* } }
// { dg-options "-O2" }

struct data {
  double x;
  double y;
  long long cond;
  double res;
};

void __RTL (startwith ("early_ra")) foo (struct data *ptr)
{
  (function "foo"
    (param "ptr"
      (DECL_RTL (reg/v:DI <0> [ ptr ]))
      (DECL_RTL_INCOMING (reg/v:DI x0 [ ptr ]))
    ) ;; param "ptr"
    (insn-chain
      (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
	(insn 4 (set (reg:DI <0>) (reg:DI x0)))
	(insn 5 (set (reg:DF <1>) (mem:DF (reg:DI <0>) [1 S8 A8])))
        (insn 6 (set (reg:DF <2>) (mem:DF (plus:DI (reg:DI <0>)
						   (const_int 8)) [1 S8 A8])))
        (insn 7 (set (reg:DI <3>) (mem:DI (plus:DI (reg:DI <0>)
						   (const_int 16)) [1 S8 A8])))
        (insn 8 (set (reg:CC cc) (compare:CC (reg:DI <3>) (const_int 0))))
        (insn 9 (set (reg:DF <4>) (reg:DF <2>)))
	(insn 10 (set (reg:DF <5>) (plus:DF (reg:DF <1>) (reg:DF <2>))))
	(insn 11 (set (reg:DF <2>) (if_then_else:DF
				     (ge (reg:CC cc) (const_int 0))
				     (reg:DF <4>)
				     (reg:DF <5>))))
        (insn 12 (set (mem:DF (plus:DI (reg:DI <0>)
				       (const_int 24)) [1 S8 A8])
		      (reg:DF <2>)))
	(edge-to exit (flags "FALLTHRU"))
      ) ;; block 2
    ) ;; insn-chain
  ) ;; function
}

int
main (void)
{
  struct data d1 = { 1, 2, -1, 0 };
  struct data d2 = { 3, 4, 1, 0 };
  foo (&d1);
  foo (&d2);
  if (d1.res != 3 || d2.res != 4)
    __builtin_abort ();
}
