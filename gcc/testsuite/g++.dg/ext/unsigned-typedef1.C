// PR c++/102804
// { dg-do compile { target c++11 } }
// { dg-options "-Wpedantic" }

using int32_t = int;
enum: unsigned int32_t { foo };	// { dg-warning "int32_t" }
int f(int) = delete;
int f(unsigned);
auto x = f(1 ? foo : 1);
