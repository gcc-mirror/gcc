// PR c++/65043
// { dg-do compile { target c++11 } }
// { dg-options "-Wnarrowing" }

struct X
{
  X(bool) { }
};

struct Y
{
  Y(char) { }
};

struct Z
{
  Z(char16_t) { }
};

struct W
{
  W(char32_t) { }
};

int main() 
{
  double d = 1.2;
  X x{d}; // { dg-warning "narrowing conversion" }
  Y y{d}; // { dg-warning "narrowing conversion" }
  Z z{d}; // { dg-warning "narrowing conversion" }
  W w{d}; // { dg-warning "narrowing conversion" }
}
