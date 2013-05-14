// PR c++/56692
// { dg-require-effective-target c++11 }

struct Class {
  void f () { }
  static void f (int) { }
};

int main ()
{
  []{ Class::f(0); };
}
