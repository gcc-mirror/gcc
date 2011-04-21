// { dg-options -std=c++0x }

struct A { ~A() = delete; };	// { dg-error "declared" }

int main()
{
  typedef const A cA[2];
  cA{};				// { dg-error "deleted" }
}
