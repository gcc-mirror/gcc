#include <altivec.h>
#include <stdarg.h>
#include <stdio.h>

typedef vector unsigned int T;

extern void f1(int, ...);
extern void f2(int, T, ...);
extern void f3(int, T, T, ...);
extern void f4(int, T, T, T);

void printx(T a)
{
  union {
    T v;
    unsigned int a[4];
  } u;
  u.v = a;
  printf("%d, %d, %d, %d\n", u.a[0], u.a[1], u.a[2], u.a[3]);
}

void f1(int a, ...)
{
  va_list ap;
  va_start (ap, a);
  while (a-- > 0)
    printx(va_arg(ap, T));
}

void f2(int a, T b, ...)
{
  va_list ap;
  printx(b);
  a--;
  va_start (ap, b);
  while (a-- > 0)
    printx(va_arg(ap, T));
}

void f3(int a, T b, T c, ...)
{
  va_list ap;
  printx(b);
  a--;
  printx(c);
  a--;
  va_start (ap, c);
  while (a-- > 0)
    printx(va_arg(ap, T));
}

void f4(int a, T b, T c,
	T d)
{
  printx(b);
  a--;
  printx(c);
  a--;
  printx(d);
  a--;
}

int main()
{
  f4 (3,
      ((T){1,1,1,1}),
      ((T){2,2,2,2}), 
      ((T){3,3,3,3}));
  f3 (3,
      ((T){4,4,4,4}),
      ((T){5,5,5,5}), 
      ((T){6,6,6,6}));
  f2 (3,
      ((T){7,7,7,7}),
      ((T){8,8,8,8}), 
      ((T){9,9,9,9}));
  f1 (3,
      ((T){10,10,10,10}),
      ((T){11,11,11,11}), 
      ((T){12,12,12,12}));
  return 0;
}

/* { dg-output "1, 1, 1, 1(\n|\r\n|\r)" }
   { dg-output "2, 2, 2, 2(\n|\r\n|\r)" }
   { dg-output "3, 3, 3, 3(\n|\r\n|\r)" }
   { dg-output "4, 4, 4, 4(\n|\r\n|\r)" }
   { dg-output "5, 5, 5, 5(\n|\r\n|\r)" }
   { dg-output "6, 6, 6, 6(\n|\r\n|\r)" }
   { dg-output "7, 7, 7, 7(\n|\r\n|\r)" }
   { dg-output "8, 8, 8, 8(\n|\r\n|\r)" }
   { dg-output "9, 9, 9, 9(\n|\r\n|\r)" }
   { dg-output "10, 10, 10, 10(\n|\r\n|\r)" }
   { dg-output "11, 11, 11, 11(\n|\r\n|\r)" }
   { dg-output "12, 12, 12, 12(\n|\r\n|\r)" }
 */
