/* An unknown CPU reports SVE2 and all reportable Armv9-A baseline features.
   None of those features should be disabled.  PREDRES has no cpuinfo token
   and must still be disabled rather than inferred from SVE2.  */
/* { dg-do compile { target { { aarch64*-*-linux* } && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_41" } */
/* { dg-additional-options "-mcpu=native" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv9-a\+crc\+nopredres\n} } } */
