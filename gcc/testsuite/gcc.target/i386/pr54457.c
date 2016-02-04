/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=short" } */

extern char array[40];

char foo (long long position)
{
  return array[position + 1];
}

/* { dg-final { scan-assembler-not "add\[lq\]?\[^\n\]*1" } } */
