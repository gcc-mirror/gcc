// { dg-do assemble  }
// { dg-prune-output "non-static data member initializers" }
// GROUPS passed initialization
class foo {
public:
  int data;
  foo(int dat) { data = dat; }
};

class bar {
public:
  foo f[3] = { 1, 2, 3 };   // works: f[0] = 1, f[1] = 2, f[2] = 3 // { dg-error "" } ANSI C++ forbids initialization of member f;
};

class bar2 {
public:
      foo f[3] = { foo(1), foo(2), foo(3) }; // { dg-error "" } ANSI C++ forbids initialization of member f;
  // does not compile -- error: field initializer is not constant
};

int main(void)
{
  foo f[3] = { foo(1), foo(2), foo(3) };
  // standard C++ ... and it works too! :)
  return 0;
}
