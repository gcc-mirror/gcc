// { dg-do compile { target c++11 } }

template <class T> T&& declval();

int main()
{
  &declval<int>();		        // { dg-error "16:taking address of rvalue" }
  declval<int>() = declval<int>();	// { dg-error "15:using rvalue as lvalue" }
  declval<int>()++;			// { dg-error "15:using rvalue as lvalue" }
  --declval<int>();			// { dg-error "17:using rvalue as lvalue" }
  declval<int>() += 1;			// { dg-error "rvalue" }
}
