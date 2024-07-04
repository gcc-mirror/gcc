// PR c++/59465
// { dg-do compile }

static const int my_size = 10;

class UserType
{
public:
  UserType(): f_(){}
private:
int f_;
};

typedef UserType Array[my_size];

class Foo
{
public:
  Foo(Array& m) : m_(m) {};  // { dg-error "invalid initializer for array member" }
private:
  Array m_;
};
