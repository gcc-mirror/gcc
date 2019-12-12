/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wno-incompatible-pointer-types" } */

struct A {
  int i;
} __attribute__ ((packed));

char* f0 (struct A *p) { return &p->i; }
