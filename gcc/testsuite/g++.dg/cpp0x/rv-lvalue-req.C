// { dg-do compile { target c++11 } }

template <class T> T&& declval();

int main()
{
  &declval<int>();		        // { dg-error "rvalue" }
  declval<int>() = declval<int>();	// { dg-error "rvalue" }
  declval<int>()++;			// { dg-error "rvalue" }
  --declval<int>();			// { dg-error "rvalue" }
  declval<int>() += 1;			// { dg-error "rvalue" }
}
