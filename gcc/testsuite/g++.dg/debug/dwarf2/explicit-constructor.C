// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++
// { dg-do compile }
// { dg-options "-O -g -dA" }
// { dg-final { scan-assembler-times "DW_AT_explicit" 2 } }

struct Foo
{
  Foo () {}
  explicit Foo (int) {}
  Foo (char) {}
  ~Foo () {};
};

void
bar ()
{
  Foo foo;
}
