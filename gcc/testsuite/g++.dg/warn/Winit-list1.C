// PR c++/67711, 48562
// { dg-do compile { target c++11 } }

#include <initializer_list>

using IL = std::initializer_list<int>;
int main()
{
  IL il = { 1,2,3 };
  il = { 4,5,6 };		// { dg-warning "initializer_list" }
  // the array is dead, il now points to garbage
  il = *new IL{ 7, 8, 9 };	// { dg-warning "initializer_list" }
  // the array is dead, il now points to garbage
  return *il.begin(); // undefined
}
