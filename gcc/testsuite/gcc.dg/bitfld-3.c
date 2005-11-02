/* Test for bitfield alignment in structs and unions.  */
/* { dg-do run { target pcc_bitfield_type_matters } }  */
/* { dg-options "-O2" }  */

extern void abort (void);
extern void exit (int);

typedef long la __attribute__((aligned (8)));

struct A
{
  char a;
  union UA
  {
    char x;
    la y : 6;
  } b;
  char c;
} a;

struct B
{
  char a;
  union UB
  {
    char x;
    long y : 6 __attribute__((aligned (8)));
  } b;
  char c;
} b;

struct C
{
  char a;
  struct UC
  {
    la y : 6;
  } b;
  char c;
} c;

struct D
{
  char a;
  struct UD
  {
    long y : 6 __attribute__((aligned (8)));
  } b;
  char c;
} d;

int main (void)
{
  if (sizeof (a) != sizeof (b))
    abort ();
  if (sizeof (a) != sizeof (c))
    abort ();
  if (sizeof (a) != sizeof (d))
    abort ();
  if ((&a.c - &a.a) != (&b.c - &b.a))
    abort ();
  if ((&a.c - &a.a) != (&c.c - &c.a))
    abort ();
  if ((&a.c - &a.a) != (&d.c - &d.a))
    abort ();
  exit (0);
}
