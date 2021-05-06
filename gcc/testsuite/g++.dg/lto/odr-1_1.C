namespace {
  struct b; // { dg-lto-note "type 'struct b' defined in anonymous namespace cannot match across the translation unit boundary" }
 }
enum vals {aa,bb,cc}; // { dg-lto-note "an enum with different value name is defined in another translation unit" }
// { dg-lto-note "mismatching definition" "" { target *-*-* } .-1 }
struct a { // { dg-lto-note "a different type is defined in another translation unit" }
  struct b *ptr; // { dg-lto-note "a field of same name but different type is defined in another translation unit" }
  enum vals vals;
} a;
void test(struct a *);
int
main(void)
{
  test (&a);
  if (a.vals==aa)
    return 1;
}
