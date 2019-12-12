/* { dg-options "-O2" } */
/* { dg-do compile } */

extern int puts (const char *);

void
f(int ch) {
  switch (ch) {
    case 3: puts("a"); break;
    case 42: puts("e"); break;
    case 333: puts("i"); break;
  } 
}

/* { dg-final { scan-assembler "cmp.*42,.*cmp.*333,.*cmp.*3," { target i?86-*-* x86_64-*-* } } } */
