namespace {
  struct b; // { dg-lto-message "type 'struct b' defined in anonymous namespace cannot match across the translation unit boundary" }
 }
enum vals {aa,bb,cc}; // { dg-lto-message "an enum with different value name is defined in another translation unit" }
struct a { // { dg-lto-message "a different type is defined in another translation unit" }
  struct b *ptr; // { dg-lto-message "a field of same name but different type is defined in another translation unit" }
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
