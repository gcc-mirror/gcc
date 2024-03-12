/* PR tree-optimization/86741 - ICE in -Warray-bounds indexing into
   an object of incomplete type
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */
/* { dg-skip-if "acessing data memory with program memory address" { "avr-*-*" } } */

struct S
{
  int s;
};

void f (void);

void test_void (void)
{
  extern void v;
  struct S *b = (struct S*)&v;
  if (b->s)
    f ();
}

void test_incomplete_enum (void)
{
  extern enum E e;
  struct S *b = (struct S*)&e;
  if (b->s)
    f ();
}

void test_func (void)
{
  struct S *b = (struct S*)&f;
  if (b->s)
    f ();
}

/* { dg-prune-output "taking address of expression of type .void." } */
