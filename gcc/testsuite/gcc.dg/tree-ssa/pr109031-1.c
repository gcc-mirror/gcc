/* { dg-do run } */
/* { dg-options "-O2" } */
unsigned char uc;
unsigned short us;

void testuc() {
  unsigned int g = 0;
  unsigned int *p1 = &g;
  unsigned char *p2 = &uc;

  do {
    (*p1)++;
    (*p2)--;
  } while (uc);

  if (g != 256)
    __builtin_abort();
}

void testus() {
  unsigned int g = 0;
  unsigned int *p1 = &g;
  unsigned short *p2 = &us;

  do {
    (*p1)++;
    (*p2)--;
  } while (us);

  if (g != 65536)
    __builtin_abort();
}

int main() {
  testuc();
  testus();
  return 0;
}

