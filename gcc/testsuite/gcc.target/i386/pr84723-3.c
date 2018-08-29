/* PR middle-end/84723 */
/* { dg-do compile } */
/* { dg-require-ifunc } */
/* { dg-options "-O2" } */

__attribute__((target_clones ("avx", "default")))
int
foo (int x)	/* { dg-error "clones for .target_clones. attribute cannot be created" } */
{		/* { dg-message "function .foo. can never be copied because it receives a non-local goto" "" { target *-*-* } .-1 } */
  __label__ lab;
  __attribute__((noinline)) void bar () { goto lab; }
  if (x == 5)
    bar ();
  x++;
lab:;
  return x;
}
