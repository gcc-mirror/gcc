//Origin: harinath@cs.umn.edu
//PR c++/10804
// G++ was not emiting the function foo.

// { dg-do run }


template<class T>
struct A
{
  A() { const void (*a)() = foo; }
  static const void foo() {}
};
int main(int argc, char *argv[])
{
  A<int> a;
}
