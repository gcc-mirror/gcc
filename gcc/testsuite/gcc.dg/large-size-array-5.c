/* { dg-do compile } */
/* { dg-options "-Wno-overflow" } */

typedef __SIZE_TYPE__ size_t;

extern char a[((size_t)-1 >> 1) + 1]; /* { dg-error "is too large" } */
extern char b[((size_t)-1 >> 1)];
extern int c[(((size_t)-1 >> 1) + 1) / sizeof(int)]; /* { dg-error "exceeds maximum object size" } */
extern int d[((size_t)-1 >> 1) / sizeof(int)];
