// { dg-lto-do link }
enum a {} b; // { dg-lto-warning "6: type 'a' violates the C\\+\\+ One Definition Rule" }
int
main(void)
{
  return 0;
}
