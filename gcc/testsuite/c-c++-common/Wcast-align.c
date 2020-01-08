/* { dg-do compile } */
/* { dg-options "-Wcast-align=strict" } */

typedef char __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) c;
typedef struct __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)))
{
  char x;
} d;

char *x;
c *y;
d *z;
struct s { long long x; } *p;
struct t { double x; } *q;

void
foo (void)
{
  y = (c *) x;  /* { dg-warning "7:cast \[^\n\r]* required alignment of target type" } */
  z = (d *) x;  /* { dg-warning "7:cast \[^\n\r]* required alignment of target type" } */
  (long long *) p;  /* { dg-bogus "alignment" } */
  (double *) q;     /* { dg-bogus "alignment" } */
}
