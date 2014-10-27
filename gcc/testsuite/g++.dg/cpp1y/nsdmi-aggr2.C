// { dg-do compile { target c++14 } }

struct S { int a; const char* b; int c; int d = b[a]; };

constexpr int f(const S& s) { return s.a; }

int main()
{
  constexpr int i = f(S{ 1, "asdf" });
}
