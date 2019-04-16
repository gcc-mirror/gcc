/* { dg-do run } */
/* { dg-options "-O" } */

struct I
{
  int i;
};

struct A
{
  struct I i1;
  struct I i2;
  struct I i3;
};

struct B
{
  struct I i0;
  struct A a;
};

struct C
{
  struct I i00;
  struct B b;
};

volatile int v;

void __attribute__((noipa))
consume_i (struct I i)
{
  v = i.i;
}

void __attribute__((noipa))
consume_a (struct A a)
{
  v = a.i1.i;
}

void __attribute__((noipa))
consume_b (struct B b)
{
  v = b.a.i1.i;
}

void __attribute__((noipa))
consume_c (struct C c)
{
  v = c.b.a.i1.i;
}




int __attribute__((noipa))
foo (struct I input)
{
  struct I i1, i2, i3;
  struct A a1, a2, a3;
  struct B b1;
  struct C c;

  i1 = input;
  a1.i1 = i1;
  b1.a = a1;

  i2.i = 1;
  b1.i0 = i2;

  c.b = b1;

  a2 = c.b.a;
  a3 = a2;
  i3 = a3.i1;

  int t = c.b.i0.i;
  a2.i3.i = 4;
  consume_i (i1);
  consume_i (i2);
  consume_b (b1);
  consume_a (a1);
  consume_a (a2);
  consume_a (a3);
  consume_c (c);

  return i3.i + t;
}

int
main (int argc, char *argv[])
{
  struct I s;
  s.i = 1234;
  int i = foo (s);
  if (i != 1235)
    __builtin_abort ();
  return 0;
}
