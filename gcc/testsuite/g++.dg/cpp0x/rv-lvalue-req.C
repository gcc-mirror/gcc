// { dg-options -std=c++0x }

template <class T> T&& declval();

int main()
{
  &declval<int>();		        // { dg-error "lvalue" }
  declval<int>() = declval<int>();	// { dg-error "lvalue" }
  declval<int>()++;			// { dg-error "lvalue" }
  --declval<int>();			// { dg-error "lvalue" }
  declval<int>() += 1;			// { dg-error "lvalue" }
}
