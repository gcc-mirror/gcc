// PR c++/38007
// We need to use the conversion function to the declared type of a bitfield,
// not the lowered bitfield type.
// { dg-do link }

struct A
{
  operator unsigned int() { return 42; }
  operator unsigned char();
};

struct B
{
  unsigned int b : 8;
};

int
main ()
{
  A u;
  unsigned int v = u;
  B w;
  w.b = u;
}
