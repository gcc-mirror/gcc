// PR c++/84559
// { dg-do compile { target c++11 } }

void foo(int i)
{
  constexpr char x[i] = "";	// { dg-error "18:.constexpr. variable .x. has variably-modified type" }
// { dg-error "18:ISO C\\+\\+ forbids variable length array .x" "" { target c++11 } .-1 }  
}
