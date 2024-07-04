/*
 * { dg-do compile }
 * { dg-options "-std=gnu23" }
 */

// conflicting attributes

extern struct [[gnu::transaction_safe]] foo { int x; } x;
extern struct [[gnu::unused]] foo2 { int x; } x2;
extern struct [[gnu::may_alias]] foo3 { int x; } x3;

void test()
{
  extern struct foo { int x; } x;		/* { dg-error "conflicting types" } */
  extern struct foo2 { int x; } x2;
  extern struct foo3 { int x; } x3;
}

