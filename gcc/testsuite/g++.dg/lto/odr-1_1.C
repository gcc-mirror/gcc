namespace {
  struct b;
 }
struct a {
  struct b *ptr;
};
void test(struct a *);
int
main(void)
{
  test (0);
}
