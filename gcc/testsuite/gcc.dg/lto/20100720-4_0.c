/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -fsigned-char} {-flto -funsigned-char}} } */

char *a;
int f;

void
foo (void)
{
  f = (*a != '-');
} 
int main()
{
  return 0;
}
