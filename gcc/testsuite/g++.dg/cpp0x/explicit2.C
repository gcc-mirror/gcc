// Test for explicit conversion ops in various conversion situations.
// { dg-options "-std=c++11" }

typedef void (*pfn)();

struct A
{
  explicit operator int() const;
  explicit operator pfn() const;
};

int main()
{
  A a;
  int i = a;			// { dg-error "" }
  const int &ir = a;		// { dg-error "" }
  a();				// { dg-error "" }
  a + 1;			// { dg-message "" } (error and note on same line)

  int j (a);
  (int)a;
  static_cast<int>(a);
}

struct B
{
  int i;
  B(const A& a): i(a) { }
};
