// PR c++/62101
// { dg-do compile { target c++11 } }

struct X
{
  friend void g(X, int) = 0; // { dg-error "initializer specified for friend function" }
  friend void g(X, int) = default; // { dg-error "cannot be defaulted" }
  // { dg-prune-output "note" }
  friend void f(X, int) = delete;
  friend void f(X, double) {}
};

struct Y;
void g(Y, int);
void g(Y, double);

struct Y
{
  // { dg-prune-output "note" }
  friend void g(Y, int) = delete;
  friend void g(Y, double) {}
};

int main()
{
  X x;
  f(x, 5.0);
  f(x, 5); // { dg-error "use of deleted function" }
  Y y;
  g(y, 5.0);
  g(y, 5); // { dg-error "use of deleted function" }
}
