// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR27574
// { dg-do compile }
// { dg-options "-O0 -gdwarf-2" }
// { dg-final { scan-assembler "problem" } }

void f (int *)
{
}

class A
{
public:
 A(int i);
};

A::A(int i)
{
 int *problem = new int(i);
 f (problem);
}

int
main (void)
{
  A a (0);

  return 0;
}

