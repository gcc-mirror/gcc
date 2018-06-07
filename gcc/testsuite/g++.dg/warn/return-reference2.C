// PR c++/26671

class A
{
public:
  int first;
  int second;

};

int &f()
{
  A a;
  return a.second;		// { dg-warning "local" }
}

int &g()
{
  int ar[42];
  return ar[20];		// { dg-warning "local" }
}
