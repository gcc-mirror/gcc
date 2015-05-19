/* Check alignment convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

#include <stddef.h>

/* Vector types get an 8 byte alignment.  */
typedef double v2df __attribute__((vector_size(16)));
typedef struct
{
  char a;
  v2df b;
} A;
char c1[offsetof (A, b) == 8 ? 0 : -1];

/* Smaller vector allow for smaller alignments.  */
typedef char v4qi __attribute__((vector_size(4)));
typedef struct
{
  char a;
  v4qi b;
} B;
char c2[offsetof (B, b) == 4 ? 0 : -1];


typedef double v4df __attribute__((vector_size(32)));
typedef struct
{
  char a;
  v4df b;
} C;
char c3[offsetof (C, b) == 8 ? 0 : -1];

/* However, we allow the programmer to chose a bigger alignment.  */
typedef struct
{
  char a;
  v2df b __attribute__((aligned(16)));
} D;
char c4[offsetof (D, b) == 16 ? 0 : -1];

typedef struct
{
  char a;
  v2df b;
} __attribute__((packed)) E;
char c5[offsetof (E, b) == 1 ? 0 : -1];
