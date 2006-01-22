// PR c++/25895
// { dg-do run }

class base {
public:
  base() {}
private:
  int val_;
};

class derived : public base {
public:
  derived() {}
};

static bool x = true ? (derived*)0 : (base*)0;

int main ()
{
  if (x)
    return 1;
}
