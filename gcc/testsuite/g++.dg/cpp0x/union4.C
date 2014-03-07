// PR c++/48537
// { dg-do compile { target c++11 } }

struct SFoo
{
  SFoo() =delete;		// { dg-message "declared" }
};

union UFoo			// { dg-error "deleted" }
{
  SFoo foo;
};

int main()
{
  UFoo();			// { dg-error "deleted" }
}
