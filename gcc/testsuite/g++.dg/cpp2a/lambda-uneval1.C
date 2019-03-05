// { dg-do compile { target c++2a } }

typedef decltype([]{}) C; // the closure type has no name for linkage purposes

// { dg-final { scan-assembler-not "globl\[ \t]*_Z1f" } }
// { dg-final { scan-assembler-not "_Z1f1C" } }
void f(C) {}

int main()
{
  C c;
  c();
  f(c);
}


