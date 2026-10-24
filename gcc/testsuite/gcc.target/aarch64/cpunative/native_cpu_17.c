/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_17" } */
/* { dg-additional-options "-march=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv9-a\+crc\+crypto\+noflagm\+nolse\+nordma\+nofp16fml\+nojscvt\+norcpc\+nofrintts\+nosb\+nopredres\+nossbs\+nopauth\n} } } */

/* Test a normal looking procinfo.  */
