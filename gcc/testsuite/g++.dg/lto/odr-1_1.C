namespace {
  struct b;
 }
struct a {
  struct b *ptr;
};
void test(struct a *); // { dg-lto-warning "6: 'test' violates the C\\+\\+ One Definition Rule" }
int
main(void)
{
  test (0);
}
