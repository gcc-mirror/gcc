/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct A {
  int i;
} __attribute__ ((packed));

void* f0 (struct A *p) { return &p->i; }
