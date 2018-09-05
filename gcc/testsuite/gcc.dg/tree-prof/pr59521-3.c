/* { dg-options "-O2 -save-temps" } */

#include <stdio.h>

__attribute__((noinline,noclone)) void
sink(const char *s) {
  asm("" :: "r"(s));
}

void
foo(int ch) {
  switch (ch) {
    case 100: sink("100"); break;
    case 10: sink("10"); break;
    case 1: sink("1"); break;
    } 
}

int main()
{
  for (int i = 0; i < 10000; i++)
  {
    int v;
    if (i % 100 == 0)
      v = 100;
    else if(i % 10 == 0)
      v = 10;
    else
      v = 1;
    foo(v);
  }
}

/* { dg-final-use-not-autofdo { scan-assembler "\nfoo:\n.*cmp.*1,.*cmp.*10,.*cmp.*100" { target i?86-*-* x86_64-*-* } } } */
