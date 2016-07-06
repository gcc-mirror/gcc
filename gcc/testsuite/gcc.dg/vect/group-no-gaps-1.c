/* { dg-require-effective-target mmap } */

#include <sys/mman.h>
#include <stdio.h>

#define COUNT 320
#define MMAP_SIZE 0x20000
#define ADDRESS1 0x1122000000
#define ADDRESS2 (ADDRESS1 + MMAP_SIZE * 16)
#define TYPE unsigned int

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

#define RHS0(B) b[B]
#define RHS1(B) RHS0(B) + b[(B) + 1]
#define RHS2(B) RHS1(B) + b[(B) + 2]
#define RHS3(B) RHS2(B) + b[(B) + 3]
#define RHS4(B) RHS3(B) + b[(B) + 4]
#define RHS5(B) RHS4(B) + b[(B) + 5]
#define RHS6(B) RHS5(B) + b[(B) + 6]
#define RHS7(B) RHS6(B) + b[(B) + 7]

#define LHS0(B) a[B]
#define LHS1(B) LHS0(B) = a[(B) + 1]
#define LHS2(B) LHS1(B) = a[(B) + 2]
#define LHS3(B) LHS2(B) = a[(B) + 3]
#define LHS4(B) LHS3(B) = a[(B) + 4]
#define LHS5(B) LHS4(B) = a[(B) + 5]
#define LHS6(B) LHS5(B) = a[(B) + 6]
#define LHS7(B) LHS6(B) = a[(B) + 7]

#define DEF_GROUP_SIZE(MULT, GAP, NO_GAP)			\
  void __attribute__((noinline, noclone))			\
  gap_load_##MULT (TYPE *__restrict a, TYPE *__restrict b)	\
  {								\
    for (int i = 0; i < COUNT; i++)				\
      a[i] = RHS##GAP (i * MULT);				\
  }								\
  void __attribute__((noinline, noclone))			\
  no_gap_load_##MULT (TYPE *__restrict a, TYPE *__restrict b)	\
  {								\
    for (int i = 0; i < COUNT; i++)				\
      a[i] = RHS##NO_GAP (i * MULT);				\
  }								\
  void __attribute__((noinline, noclone))			\
  gap_store_##MULT (TYPE *__restrict a, TYPE *__restrict b)	\
  {								\
    for (int i = 0; i < COUNT; i++)				\
      LHS##GAP (i * MULT) = b[i];				\
  }								\
  void __attribute__((noinline, noclone))			\
  no_gap_store_##MULT (TYPE *__restrict a, TYPE *__restrict b)	\
  {								\
    for (int i = 0; i < COUNT; i++)				\
      LHS##NO_GAP (i * MULT) = b[i];				\
  }

#define USE_GROUP_SIZE(MULT)					\
  gap_load_##MULT (end_x - COUNT, end_y - COUNT * MULT + 1);	\
  no_gap_load_##MULT (end_x - COUNT, end_y - COUNT * MULT);	\
  gap_store_##MULT (end_x - COUNT * MULT + 1, end_y - COUNT);	\
  no_gap_store_##MULT (end_x - COUNT * MULT, end_y - COUNT)

DEF_GROUP_SIZE (2, 0, 1)
DEF_GROUP_SIZE (3, 1, 2)
DEF_GROUP_SIZE (4, 2, 3)
DEF_GROUP_SIZE (5, 3, 4)
DEF_GROUP_SIZE (6, 4, 5)
DEF_GROUP_SIZE (7, 5, 6)
DEF_GROUP_SIZE (8, 6, 7)

int
main (void)
{
  void *x, *y;
  TYPE *end_x, *end_y;

  x = mmap ((void *) ADDRESS1, MMAP_SIZE, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (x == MAP_FAILED)
    {
      perror ("mmap");
      return 1;
    }

  y = mmap ((void *) ADDRESS2, MMAP_SIZE, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (y == MAP_FAILED)
    {
      perror ("mmap");
      return 1;
    }

  end_x = (TYPE *) ((char *) x + MMAP_SIZE);
  end_y = (TYPE *) ((char *) y + MMAP_SIZE);

  USE_GROUP_SIZE (2);
  USE_GROUP_SIZE (3);
  USE_GROUP_SIZE (4);
  USE_GROUP_SIZE (5);
  USE_GROUP_SIZE (6);
  USE_GROUP_SIZE (7);
  USE_GROUP_SIZE (8);

  return 0;
}
