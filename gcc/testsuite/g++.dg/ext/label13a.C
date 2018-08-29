// PR c++/41090
// { dg-do run }
// { dg-options "-save-temps" }
// { dg-require-effective-target indirect_jumps }
// { dg-final { scan-assembler "_ZN1CC4Ev" } }

int i;
struct A { A() {} };
struct C: virtual A
{
  C();
};

C::C()
{
  static void *labelref = &&label;
  goto *labelref;
 label: i = 1;
}

int main()
{
  C c;
  return (i != 1);
}
