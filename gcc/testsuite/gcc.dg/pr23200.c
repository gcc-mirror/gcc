/* PR inline-asm/23200 */
/* { dg-do compile { target nonpic } } */
/* { dg-options "-O0" } */

static char var;

void
foo (void)
{
  asm volatile ("" :: "i" (&var + 1));
}

typedef int T[];
typedef T *P;

int var2;

void
bar (void)
{
  asm volatile ("" :: "i"(&(*(P)&var2)[1]));
}
