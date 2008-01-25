/* { dg-do run } */

extern "C" void abort() __attribute__ ((noreturn));

struct s
{
  unsigned long long f1 : 40;
  unsigned int f2 : 24;
} sv;

int main()
{
  int f2;
  sv.f2 = (1 << 24) - 1;
  __asm__ volatile ("" : : : "memory");
  ++sv.f2;
  f2 = sv.f2;
  if (f2 != 0)
    abort();
  return 0;
}
