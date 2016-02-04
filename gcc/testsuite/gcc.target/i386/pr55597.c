/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC -mx32 -maddress-mode=long" } */

struct initial_sp
{
  void *sp;
  int mask;
};

__thread struct initial_sp __morestack_initial_sp;

void foo (int *);

void __morestack_release_segments (void)
{
  foo (&__morestack_initial_sp.mask);
}
