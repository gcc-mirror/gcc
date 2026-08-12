/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables this fusion pair yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LOGIC_LOGIC" 6 "sched2" { xfail *-*-* } } } */

typedef long int64_t;

/* Register source followed by an immediate source.  */
int64_t
test_and_xori (int64_t a, int64_t b)
{
  return (a & b) ^ 0x1f;
}

/* Immediate source followed by a register source.  */
int64_t
test_andi_xor (int64_t a, int64_t b)
{
  return (a & 0x55) ^ b;
}

/* Complemented first operation followed by an immediate operation.  */
int64_t
test_andn_ori (int64_t a, int64_t b)
{
  return (~a & b) | 0x33;
}

/* Immediate first operation followed by a complemented operation.  */
int64_t
test_ori_xnor (int64_t a, int64_t b)
{
  return ~((a | 0x33) ^ b);
}

/* Unary first operation followed by an immediate operation.  Use RTL to
   prevent combine from replacing the pair with andn.  */
int64_t __RTL (startwith ("sched2"))
test_not_andi (void)
{
(function "test_not_andi"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (not:DI (reg:DI a1))))
      (cinsn 4 (set (reg:DI a0)
                    (and:DI (reg:DI a0)
                            (const_int 85))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_not_andi"
}

/* Immediate first operation followed by a unary operation.  */
int64_t
test_andi_not (int64_t a)
{
  return ~(a & 0x55);
}
