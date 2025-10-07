/* Test C2y constraint for incomplete types of objects with no linkage.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s1;

void
f ()
{
  struct s1 a; /* { dg-error "storage size" } */
  int b[]; /* { dg-error "array size missing" } */
  static struct s1 c; /* { dg-error "storage size" } */
  static int d[]; /* { dg-error "storage size" } */
  struct s2;
  struct s2 e; /* { dg-error "storage size" } */
  static struct s2 g; /* { dg-error "storage size" } */
  struct s2 { int i; };
}

struct s1 { int j; };
