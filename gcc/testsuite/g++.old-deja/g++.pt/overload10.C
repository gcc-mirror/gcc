// { dg-do run  }
struct B {
  int f(int) { return 1; }
};

struct D {
  template <class T>
  int f(T) { return 0; }
};

int main()
{
  int (D::*g)(int) = &D::f;
  
  D d;
  return (d.*g)(0);
}
