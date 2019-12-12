/* { dg-options "-O2 -fipa-sra -fno-inline -fno-ipa-cp"  } */


char *a() __attribute__((__malloc__));
static char *b() {
  char *c = a();
  return c;
}
int d() { b(); return 4; }
