/* { dg-do run } */

char *volatile pa;
char *volatile pb;
char *volatile pc;
typedef __UINTPTR_TYPE__ uintptr_t;

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
  if ((uintptr_t) pb == (uintptr_t) pa + 32)
    access (pa + 32);
  else if ((uintptr_t) pb == (uintptr_t) pa - 32)
    access (pa - 32);
  else if ((uintptr_t) pb == (uintptr_t) pa + 64)
    access (pa + 64);
  else if ((uintptr_t) pb == (uintptr_t) pa - 64)
    access (pa - 64);

  return 0;
}
