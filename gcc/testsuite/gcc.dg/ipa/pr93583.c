/* PR ipa/93583 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void *bar(const char*);
static void *__attribute__((malloc,noinline)) foo(const char *p)
{
  return bar (p);
}

int main(int argc, char **argv)
{
  foo (argv[0]);
  return 0;
}
