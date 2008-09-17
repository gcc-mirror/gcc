// PR c++/26671

class A
{
public:
  int first;
  int second;

};

int &f()
{
  A a;				// { dg-warning "local" }
  return a.second;
}

int &g()
{
  int ar[42];			// { dg-warning "local" }
  return ar[20];
}
