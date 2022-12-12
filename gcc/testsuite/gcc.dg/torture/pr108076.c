/* { dg-do link } */

static void *j;
int v, g;
__attribute__((__leaf__)) int atoi (const char *);

int
main ()
{
  j = &&lab1;
  &&lab2;
  atoi ("42");
lab1:
lab2:
  if (v)
    goto *j;
}
