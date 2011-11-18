/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining" } */

class A
{
public:
  int a;
  void *stuff;
};

class B
{
public:
  int b;
  void *other_stuff;
  A array[50];
};

extern B gb;

int process_A (A *a)
{
  return a->a;
}

int process_A_complex (A *a)
{
  return process_A (a+3);
}

int process_B (B *b)
{
  return process_A_complex (&b->array[0]);
}

int foo (void)
{
  return process_B (&gb);
}

