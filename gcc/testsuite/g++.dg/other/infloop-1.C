// PR 18300: This sends old compilers into an infinite loop on x86_64
// Testcase and patch contributed by Zak Kipling <zak@transversal.com>

struct base1 { };
struct base2 { };
struct base3 { };

struct derived : base1, base2, base3 { };

void foo(derived);

int main()
{
  foo(derived());
}

