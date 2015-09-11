/* PR middle-end/36043 target/58744 target/65408 */
/* { dg-do run { target mmap } } */
/* { dg-options "-O2" } */

#include <sys/mman.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
#ifndef MAP_ANON
#define MAP_ANON 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

typedef struct
{
  unsigned char r;
  unsigned char g;
  unsigned char b;
} __attribute__((packed)) pr58744;

typedef struct
{
  unsigned short r;
  unsigned short g;
  unsigned short b;
} pr36043;

typedef struct
{
  int r;
  int g;
  int b;
} pr65408;

__attribute__ ((noinline, noclone))
void
f1a (pr58744 x)
{
  if (x.r != 1 || x.g != 2 || x.b != 3)
    __builtin_abort();
}

__attribute__ ((noinline, noclone))
void
f1 (pr58744 *x)
{
  f1a (*x);
}

__attribute__ ((noinline, noclone))
void
f2a (pr36043 x)
{
  if (x.r != 1 || x.g != 2 || x.b != 3)
    __builtin_abort();
}

__attribute__ ((noinline, noclone))
void
f2 (pr36043 *x)
{
  f2a (*x);
}

__attribute__ ((noinline, noclone))
void
f3a (pr65408 x)
{
  if (x.r != 1 || x.g != 2 || x.b != 3)
    __builtin_abort();
}

__attribute__ ((noinline, noclone))
void
f3 (pr65408 *x)
{
  f3a (*x);
}

int
main ()
{
  char *p = mmap ((void *) 0, 131072, PROT_READ | PROT_WRITE,
		  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED)
    return 0;
  char *endp = p + 65536;
  if (munmap (endp, 65536) < 0)
    return 0;

  pr58744 *s1 = (pr58744 *) endp - 1;
  s1->r = 1;
  s1->g = 2;
  s1->b = 3;
  f1 (s1);

  pr36043 *s2 = (pr36043 *) endp - 1;
  s2->r = 1;
  s2->g = 2;
  s2->b = 3;
  f2 (s2);

  pr65408 *s3 = (pr65408 *) endp - 1;
  s3->r = 1;
  s3->g = 2;
  s3->b = 3;
  f3 (s3);

  return 0;
}
