// { dg-do compile { target c++2a } }

#include <compare>

#define vector __attribute__((vector_size(4*sizeof(int)) ))

int main()
{
  vector int a, b;
  a <=> b;		     // { dg-message "three-way comparison of vector" }
}
