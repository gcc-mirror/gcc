/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

w *x; /* { dg-error "unknown type name 'w'" } */

int z;    /* { dg-message "previous declaration of 'z'" } */
y         /* { dg-error "unknown type name 'y'" } */
  * z;    /* { dg-error "conflicting " } */

int f1()
{
  int d, e;
  d * e; /* { dg-bogus "unknown type name 'd'" } */
  g * h; /* { dg-error "unknown type name 'g'" } */
  g i;   /* { dg-error "unknown type name 'g'" } */
}

typedef int a;

int f2()
{
b: a: ; /* { dg-bogus "a label can only be part of a statement" } */
c: d e; /* { dg-error "a label can only be part of a statement" } */
/* { dg-error "unknown type name 'd'" "unknown type name" { target *-*-* } .-1 } */
   ;
}

void *f3()
{
  return x; /* { dg-bogus "'x' undeclared" } */
}

