/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_37" } */
/* Force the architecture detection path with an unknown MIDR.  SVE2
   suggests Armv9, but does not establish its other baseline features.  */
/* { dg-additional-options "-march=native -mtune=cortex-a34" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv9-a\+crc\+crypto\+sve2p1\+noflagm\+nolse\+nordma\+nofp16fml\+nojscvt\+norcpc\+nofrintts\+nosb\+nopredres\+nossbs\+nopauth\n} } } */
