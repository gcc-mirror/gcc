/* { dg-require-stack-size "4096" } */

int foo (__const char *__restrict __s);
static void 
read_anisou(char line[])
{
  foo (line+1);
}
void
read_pdbfile(void)
{
  char line[4096];
  read_anisou (line);
}
