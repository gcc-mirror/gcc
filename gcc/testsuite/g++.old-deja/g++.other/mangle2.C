// Test for overloaded operators in "C" linkage
// Build don't link:

extern "C" {
typedef struct b
{
  int a;
} c;

extern const c z;

inline bool operator!=(const c& x, const c& y)
{
  return x.a != y.a;
}
};

void foo();

void bar(c x)
{
  if (x != z)
    foo();
}
