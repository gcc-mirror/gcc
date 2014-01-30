/* PR c/12085 */
/* Origin: David Hollenberg <dhollen@mosis.org> */

/* Verify that the compiler doesn't inline a function at
   a calling point where it is viewed with a different
   prototype than the actual one.  */

/* { dg-do compile } */
/* { dg-options "-O3" } */

int foo1(int);
int foo2();

typedef struct {
  double d;
  int a;
} str_t;

void bar(double d, int i, str_t s)
{
  d = ((double (*) (int)) foo1) (i);  /* { dg-warning "33:non-compatible|abort" } */
  i = ((int (*) (double)) foo1) (d);  /* { dg-warning "33:non-compatible|abort" } */
  s = ((str_t (*) (int)) foo1) (i);   /* { dg-warning "32:non-compatible|abort" } */
  ((void (*) (int)) foo1) (d);        /* { dg-warning "non-compatible|abort" } */
  i = ((int (*) (int)) foo1) (i);     /* { dg-bogus "non-compatible|abort" } */
  (void) foo1 (i);                    /* { dg-bogus "non-compatible|abort" } */

  d = ((double (*) (int)) foo2) (i);  /* { dg-warning "33:non-compatible|abort" } */
  i = ((int (*) (double)) foo2) (d);  /* { dg-bogus "non-compatible|abort" } */
  s = ((str_t (*) (int)) foo2) (i);   /* { dg-warning "non-compatible|abort" } */
  ((void (*) (int)) foo2) (d);        /* { dg-warning "non-compatible|abort" } */
  i = ((int (*) (int)) foo2) (i);     /* { dg-bogus "non-compatible|abort" } */
  (void) foo2 (i);                    /* { dg-bogus "non-compatible|abort" } */
}

int foo1(int arg)
{
  /* Prevent the function from becoming const and thus DCEd.  */
  __asm volatile ("" : "+r" (arg));
  return arg;
}

int foo2(arg)
  int arg;
{
  /* Prevent the function from becoming const and thus DCEd.  */
  __asm volatile ("" : "+r" (arg));
  return arg;
}
