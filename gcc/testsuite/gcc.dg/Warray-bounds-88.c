/* PR middle-end/101977 - bogus -Warray-bounds on a negative index into
   a parameter in conditional with null
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct A { int i; };
struct B { struct A a1; struct A a2; };


void nowarn_p_0_0 (struct A *p, int i)
{
  struct A *q = i < 0 ? p : 0 < i ? (struct A*)0 : 0;
  struct B *r = (struct B*)((char *)q - __builtin_offsetof (struct B, a2));
  r->a1.i = 0;
}

void nowarn_0_p_0 (struct A *p, int i)
{
  struct A *q = i < 0 ? 0 : 0 < i ? p : 0;
  struct B *r = (struct B*)((char *)q - __builtin_offsetof (struct B, a2));
  r->a1.i = 0;      // { dg-bogus "-Warray-bounds" }
}

void nowarn_0_0_p (struct A *p, int i)
{
  struct A *q = i < 0 ? 0 : 0 < i ? 0 : p;
  struct B *r = (struct B*)((char *)q - __builtin_offsetof (struct B, a2));
  r->a1.i = 0;      // { dg-bogus "-Warray-bounds" }
}


void nowarn_p_q_0 (struct A *p, struct A *q, int i)
{
  struct A *r = i < 0 ? p : 0 < i ? q : 0;
  struct B *s = (struct B*)((char *)r - __builtin_offsetof (struct B, a2));
  s->a1.i = 0;      // { dg-bogus "-Warray-bounds" }
}

void nowarn_p_0_q (struct A *p, struct A *q, int i)
{
  struct A *r = i < 0 ? p : 0 < i ? 0 : q;
  struct B *s = (struct B*)((char *)r - __builtin_offsetof (struct B, a2));
  s->a1.i = 0;      // { dg-bogus "-Warray-bounds" }
}

void nowarn_0_p_q (struct A *p, struct A *q, int i)
{
  struct A *r = i < 0 ? 0 : 0 < i ? p : q;
  struct B *s = (struct B*)((char *)r - __builtin_offsetof (struct B, a2));
  s->a1.i = 0;
}


void nowarn_p_q_r (struct A *p, struct A *q, struct A *r, int i)
{
  struct A *s = i < 0 ? p : 0 < i ? q : r;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}


extern struct B b1, b2, b3;

void nowarn_p_b1_0 (struct A *p, int i)
{
  struct A *r = i < 0 ? p : 0 < i ? &b1.a2 : 0;
  struct B *s = (struct B*)((char *)r - __builtin_offsetof (struct B, a2));
  s->a1.i = 0;      // { dg-bogus "-Warray-bounds" }
}

void nowarn_p_0_b1 (struct A *p, int i)
{
  struct A *r = i < 0 ? p : 0 < i ? 0 : &b1.a2;
  struct B *s = (struct B*)((char *)r - __builtin_offsetof (struct B, a2));
  s->a1.i = 0;      // { dg-bogus "-Warray-bounds" }
}

void nowarn_0_p_b1 (struct A *p, int i)
{
  struct A *r = i < 0 ? 0 : 0 < i ? p : &b1.a2;
  struct B *s = (struct B*)((char *)r - __builtin_offsetof (struct B, a2));
  s->a1.i = 0;
}


void nowarn_p_b1_b2 (struct A *p, int i)
{
  struct A *s = i < 0 ? p : 0 < i ? &b1.a2 : &b2.a2;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}

void nowarn_b1_p_b2 (struct A *p, int i)
{
  struct A *s = i < 0 ? &b1.a2 : 0 < i ? p : &b2.a2;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}

void nowarn_b1_b2_p (struct A *p, int i)
{
  struct A *s = i < 0 ? &b1.a2 : 0 < i ? &b2.a2 : p;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}

void nowarn_b1_b2_b3 (struct A *p, int i)
{
  struct A *s = i < 0 ? &b1.a2 : 0 < i ? &b2.a2 : &b3.a2;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}


void nowarn_0_b1_b2 (int i)
{
  struct A *s = i < 0 ? 0 : 0 < i ? &b1.a2 : &b2.a2;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}

void warn_b1_0_b2 (int i)
{
  struct A *s = i < 0 ? &b1.a2 : 0 < i ? 0 : &b2.a2;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}

void warn_b1_b2_0 (int i)
{
  struct A *s = i < 0 ? &b1.a2 : 0 < i ? &b2.a2 : 0;
  struct B *t = (struct B*)((char *)s - __builtin_offsetof (struct B, a2));
  t->a1.i = 0;
}
