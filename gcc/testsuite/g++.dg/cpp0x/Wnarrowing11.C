// PR c++/65043
// { dg-do compile { target c++11 } }

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
  X x{1.2}; // { dg-error "narrowing conversion" }
  Y y{1.2}; // { dg-error "narrowing conversion" }
  Z z{1.2}; // { dg-error "narrowing conversion" }
  W w{1.2}; // { dg-error "narrowing conversion" }
}
