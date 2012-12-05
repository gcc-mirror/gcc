/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -w } } } */

char s[8];
int main(void)
{
  return strcmp(&s[1], "foo");
}
