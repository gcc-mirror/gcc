// PR c++/33984
// { dg-do compile }

struct S
{
  unsigned int bar : 3;
} s;

int foo (unsigned int &);
int foo (double);

int
main ()
{
  return foo (s.bar);		// { dg-error "cannot bind bit-field" }
}
