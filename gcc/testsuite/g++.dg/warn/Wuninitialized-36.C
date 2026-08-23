// PR tree-optimization/127005
// { dg-do compile { target c++17 } }
// { dg-options "-O2 -W -Wall" }

#include <optional>

void f(int *);
struct s1
{
  int *a;
  ~s1()
  {
     f(a); // { dg-bogus "uninitialized" }
  }
};

struct conn {
        void close() { guard.reset(); }
        std::optional<s1> guard;
};
extern conn make_conn();
int main()
{
        conn c = make_conn();
        c.close();
}
