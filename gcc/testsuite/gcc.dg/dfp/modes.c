/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

typedef float decimal32  __attribute__ ((mode (SD)));
typedef float decimal64  __attribute__ ((mode (DD)));
typedef float decimal128 __attribute__ ((mode (TD)));

int ssize[sizeof (decimal32) == 4 ? 1 : -1];
int dsize[sizeof (decimal64) == 8 ? 1 : -1];
int tsize[sizeof (decimal128) == 16 ? 1 : -1];

int salign = __alignof (decimal32);
int dalign = __alignof (decimal64);
int talign = __alignof (decimal128);

