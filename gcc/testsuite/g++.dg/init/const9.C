// PR c++/55893
// { dg-final { scan-assembler-not "rodata" { target { ! "hppa*-*-* s390*-*-*" } } } }

struct foo
{
  virtual ~foo ();
};

int main ()
{
  static const foo tmp;
}
