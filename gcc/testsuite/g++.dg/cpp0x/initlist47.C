// { dg-options -std=c++11 }

struct A { ~A() = delete; };	// { dg-message "declared" }

int main()
{
  typedef const A cA[2];
  cA{};				// { dg-error "deleted" }
}
