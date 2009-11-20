/* { dg-options "-Warray-bounds" } */
/* { dg-do compile } */

struct A
{
  int i;
  char p[1];
};

struct B
{
  struct A a;
  int i;
};

struct C
{
  int i;
  struct A a;
};

union D
{
  char p[1];
  struct A a;
  struct B b;
  struct C c;
};

struct E
{
  int i;
  union D d;
};

struct F
{
  union D d;
  int i;
};

union G
{
  int i;
  union D d;
};

void
f0 ()
{
  __builtin_offsetof (struct A, p[4]); /* OK */
  __builtin_offsetof (struct B, a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (struct C, a.p[4]); /* OK */
  __builtin_offsetof (union D, p[4]); /* OK */
  __builtin_offsetof (union D, a.p[4]); /* OK */
  __builtin_offsetof (union D, b.a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (union D, c.a.p[4]); /* OK */
  __builtin_offsetof (struct E, d.p[4]); /* OK */
  __builtin_offsetof (struct E, d.a.p[4]); /* OK */
  __builtin_offsetof (struct E, d.b.a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (struct E, d.c.a.p[4]); /* OK */
  __builtin_offsetof (struct F, d.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (struct F, d.a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (struct F, d.b.a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (struct F, d.c.a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (union G, d.p[4]); /* OK */
  __builtin_offsetof (union G, d.a.p[4]); /* OK */
  __builtin_offsetof (union G, d.b.a.p[4]); /* { dg-warning "greater than size" } */
  __builtin_offsetof (union G, d.c.a.p[4]); /* OK */
}
