// PR c++/97296
// { dg-do compile }

void f(const int * const &);
void f(const int *);
int *x;
int main()
{
  f(x); // { dg-error "call of overloaded" }
}
