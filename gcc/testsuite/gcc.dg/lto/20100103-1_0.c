/* { dg-lto-do link } */
/* { dg-lto-options {{-funsigned-char -flto} {-fsigned-char -flto}} } */

char *foo;
int main()
{
  foo = "bar";
}
