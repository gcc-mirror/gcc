/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-details -fdump-rtl-pro_and_epilogue" } */
/* PR tree-optimization/46555 */
/* Here should not remove the forwarder block (or rather recreate it and not
   remove it again). This improves expansion to RTL as there is one less copy
   (or constant formation) in some cases. In this case we also get the ability
   to shrink wrap the function.  */

int h(void);
int f(int a, int b, int c)
{
  if (a)
    return 2;
  h();
  if (b)
    return 2;
  h();
  if (c)
    return 2;
  h();
  return 4;
}

/* { dg-final { scan-tree-dump-times "New forwarder block for edge" 1 "optimized" } } */
/* Make sure we only have a PHI with 2 arguments here, 2 and 4.  */
/* { dg-final { scan-tree-dump "PHI <2..., 4...>|PHI <4..., 2...>" "optimized" } } */
/* Make sure we can shrink wrap the function now too. */
/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" { target { { { i?86-*-* x86_64-*-* } && { ! ia32 } } || { powerpc*-*-* aarch64*-*-* riscv*-*-* arm*-*-* }  } } } } */
