// { dg-lto-do link }
// { dg-lto-options { { -O0 -flto }  } 
enum a {} b; // { dg-lto-warning "6: type 'a' violates the C\\+\\+ One Definition Rule" }
int
main(void)
{
  return 0;
}
