/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { ! global_constructor } } */

/* The ipa-split pass pulls the body of the if(!x) block
   into a separate function to make foo a better inlining
   candidate.  Make sure this new function isn't also run
   as a static constructor.  */

#include <stdlib.h>

int x, y;

void __attribute__((noinline))
bar(void)
{
  y++;
}

void __attribute__((constructor))
foo(void)
{
  if (!x)
    {
      bar();
      y++;
    }   
} 

int main()
{
  x = 1;
  foo();
  foo();
  if (y != 2)
    abort();
  exit(0);
}
