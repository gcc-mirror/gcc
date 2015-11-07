/* { dg-lto-do run } */
/* { dg-lto-options { { -O0 -flto -fno-math-errno } } } */
/* { dg-lto-options { "-O0 -flto -fno-math-errno -mfloat-abi=softfp -mfpu=neon-vfpv4" } { target arm*-*-* } } */
/* { dg-require-linker-plugin "" } */
/* { dg-require-effective-target sqrt_insn } */

/* We require a linker plugin because otherwise we'd need to link
   against libm which we are not sure here has cabs on all targets.
   This is because collect2 invokes ld on the -O0 object code
   which does not have folded cabs.  */

double cabs(_Complex double);
double __attribute__((used)) __attribute__ ((optimize ("O2,fast-math")))
foo (_Complex double x, int b)
{
  if (b)
    x = 0;
  return cabs(x);
}

int main() { return 0; }
