/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

unsigned int
foo32 (unsigned int x)
{
  return (((x << 24) & 0xff000000)
	  | ((x << 8) & 0xff0000)
	  | ((x >> 8) & 0xff00)
	  | ((x >> 24) & 0xff));
}

unsigned int
foo32_1 (unsigned int x)
{
  return __builtin_bswap32 (x);
}

#if __riscv_xlen == 64
unsigned long
foo64 (unsigned long x)
{
  return (((x << 56) & 0xff00000000000000ull)
	  | ((x << 40) & 0xff000000000000ull)
	  | ((x << 24) & 0xff0000000000ull)
	  | ((x << 8) & 0xff00000000ull)
	  | ((x >> 8) & 0xff000000)
	  | ((x >> 24) & 0xff0000)
	  | ((x >> 40) & 0xff00)
	  | ((x >> 56) & 0xff));
}

unsigned long
foo64_1 (unsigned long x)
{
  return __builtin_bswap64 (x);
}
#endif

/* { dg-final { scan-assembler-times "th.rev\t" 2 { target { rv32 } } } } */

/* { dg-final { scan-assembler-times "th.revw\t" 2 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.rev\t" 2 { target { rv64 } } } } */
