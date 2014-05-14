// PR c++/21631

int f(int&);
int f();

int g(void)
{
  return f(1);			// { dg-error "rvalue" }
}
