// { dg-options "-Wabi-tag" }

// { dg-final { scan-assembler "_Z1fB3barB3fooi" } }
void f(int) __attribute ((abi_tag ("foo","bar")));

struct __attribute ((abi_tag ("bar"))) A { };

struct B: A { };		// { dg-warning "bar. ABI tag" }
struct D { A* ap; };		// { dg-warning "bar. ABI tag" }

// { dg-final { scan-assembler "_Z1gB3baz1AB3bar" } }
void g(A) __attribute ((abi_tag ("baz")));
void g(A) __attribute ((abi_tag ("baz")));

int main()
{
  f(42);
  g(A());
}
