// PR c++/48537
// { dg-options -std=c++0x }

struct SFoo
{
  SFoo() =delete;		// { dg-error "declared" }
};

union UFoo			// { dg-error "deleted" }
{
  SFoo foo;
};

int main()
{
  UFoo();			// { dg-error "deleted" }
}
