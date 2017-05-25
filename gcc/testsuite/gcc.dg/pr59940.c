/* PR c/59940 */
/* { dg-do compile } */
/* { dg-options "-Wconversion -Woverflow" } */

int f (unsigned int);
typedef int sitype __attribute__((mode(SI)));

int
g (void)
{
  sitype si = 12;
  unsigned int ui = -1; /* { dg-warning "21:-Wsign-conversion" } */
  unsigned char uc;
  ui = si; /* { dg-warning "8:conversion" } */
  si = 0x80000000; /* { dg-warning "8:-Wsign-conversion" } */
  si = 3.2f; /* { dg-warning "8:conversion" } */
  uc = 256; /* { dg-warning "8:-Woverflow" } */
  si = 0x800000000; /* { dg-warning "8:-Woverflow" } */
  return f (si) /* { dg-warning "13:conversion" } */
         + f (si); /* { dg-warning "15:conversion" } */
}

int
y (void)
{
  f (); /* { dg-error "3:too few arguments to function" } */
  g (0xa); /* { dg-error "3:too many arguments to function" } */
}
