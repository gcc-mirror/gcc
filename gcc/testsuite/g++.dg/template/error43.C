// PR c++/29363

template<int> void foo()
{
  throw A();			// { dg-message "declar" }
  struct A {} a;
}

template void foo<0>();		// { dg-message "instantiated" }
