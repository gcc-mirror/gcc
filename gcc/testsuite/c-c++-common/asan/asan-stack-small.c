/* { dg-do run } */

char *pa;
char *pb;
char *pc;

void access (volatile char *ptr)
{
  *ptr = 'x';
}

int main (int argc, char **argv)
{
  char a;
  char b;
  char c;

  pa = &a;
  pb = &b;
  pc = &c;

  access (pb);
  access (pc);
  // access 'b' here
  access (pa + 32);

  return 0;
}
