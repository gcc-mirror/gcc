/* { dg-options "-fstrict-volatile-bitfields" } */
/* { dg-do run } */

extern int puts(const char *);
extern void abort(void) __attribute__((noreturn));

typedef struct {
  volatile unsigned short a:8, b:8;
} BitStruct;

BitStruct bits = {1, 2};

void check(int i, int j)
{
  if (i != 1 || j != 2) puts("FAIL"), abort();
}

int main ()
{
  check(bits.a, bits.b);

  return 0;
}
