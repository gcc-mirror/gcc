// PR c++/47504
// { dg-do compile { target c++11 } }

char constexpr sub(char arg)
{ return char(arg - char(1)); }

int main()
{ static char constexpr m = sub(-1); }
