// PR c++/66617
// { dg-do compile { target c++11 } }

struct A { };
struct B: virtual A
{
protected:
  B(int, int);
};
struct C: B
{
  C(): B{1,2} {}
};

       
