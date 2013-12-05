/* { dg-do compile { target ia64-*-* } } */
/* { dg-options "-O2" } */

char* area;
long int area_size;
char* base;

void fun(unsigned long int addr)
{
  unsigned long int size32 = (addr + 4096 - 1) & ~(4096 - 1);
  unsigned long int size = size32 * sizeof(unsigned int);

  if (size > 0) {
    size = (size + 1) & ~(1);
  }

  area_size = size;
  area = base + size;
}
