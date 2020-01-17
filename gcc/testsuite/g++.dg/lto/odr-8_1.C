struct a {char c; a() {} a(struct a &) {}}; // { dg-lto-message "one type needs to be constructed while other not" }
extern int test (struct a *a);
int
main()
{
  struct a a;
  a.c=0;
  return test(&a);
}
