// PR c++/55893
// { dg-final { scan-assembler-not "rodata" } }

struct foo
{
  virtual ~foo ();
};

int main ()
{
  static const foo tmp;
}
