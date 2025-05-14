/* { dg-do compile } */
/* { dg-options "-O2"  } */
/* { dg-additional-options "-msse2" { target ia32 } } */
/* { dg-additional-options "-Wno-psabi" { target powerpc-ibm-aix* powerpc-wrs-vxworks* powerpc-*-elf } } */

typedef int __attribute__((__vector_size__(16))) vectype;

vectype dk();
vectype k(vectype);

int b;
vectype *j;
inline int c(vectype *d) {
  vectype e;
  vectype f;
  vectype g = *d;
  vectype h = g;
  vectype i = h;
  f = i == dk();
  e = f == b;
  k(e);
}

static void m(vectype *d) {
  int l = c(d);
  if (l)
    c(j);
}

void o(void) {
  vectype n;
  m(&n);
}
