// PR middle-end/43631
// { dg-do compile }
// { dg-options "-g -O2" }
// { dg-additional-options "-mtune=atom" { target i?86-*-* x86_64-*-* } }

typedef void (*T) ();
struct S { T t; };
void bar (T) __attribute__ ((__noreturn__));
S *p;

void
foo ()
{
  try { bar (p->t); } catch (...) { throw 1; }
}
