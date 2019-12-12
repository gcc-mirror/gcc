// { dg-do run { target c++17 } }

struct derived;
struct base { };
struct derived : base {
  int i;
};

bool flag;
base f() {
  flag = true;
  return base();
}

derived d2{f(),1};

int main()
{
  return (!flag || d2.i != 1);
}
