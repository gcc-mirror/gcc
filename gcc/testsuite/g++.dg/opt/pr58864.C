// PR target/58864
// { dg-do compile }
// { dg-options "-Os" }
// { dg-additional-options "-march=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } }

struct A { A (); ~A (); };
struct B { B (); };

float d, e;

void
foo ()
{
  A a;
  float c = d;
  while (1)
    {
      B b;
      e = c ? -c : 0;
    }
}
