// PR c++/28523

class A {};
class B : public A {};

int main()
{
  throw (A) B();
  return 0;
}
