/* PR middle-end/91977 - missing -Wstringop-overflow on memcpy into
   a pointer plus offset
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#define NOIPA          __attribute__ ((noipa))
#define CONCAT(a, b)   a ## b
#define CAT(a, b)      CONCAT (a, b)

#define S3 "123"
#define S4 "1234"

char a1[1], a2[2], a3[3], a4[4], a5[5], a6[6], a7[7], a8[8];
char b1[1], b2[2], b3[3], b4[4], b5[5], b6[6], b7[7], b8[8];

#define T(dst, src, off, n)					\
  NOIPA void CAT (test_on_line_, __LINE__) (const void *s)	\
  {								\
    __builtin_memcpy (dst + off, src, n);			\
  } typedef void dummy_type

T (a4, s, 0, 1);
T (a4, s, 1, 1);
T (a4, s, 2, 1);
T (a4, s, 3, 1);
T (a4, s, 4, 1);    // { dg-warning "writing 1 byte into a region of size 0" }

T (a4, s, 0, 2);
T (a4, s, 1, 2);
T (a4, s, 2, 2);
T (a4, s, 3, 2);    // { dg-warning "writing 2 bytes into a region of size 1" }
T (a4, s, 4, 2);    // { dg-warning "writing 2 bytes into a region of size 0" }

T (a4, s, 0, 3);
T (a4, s, 1, 3);
T (a4, s, 2, 3);    // { dg-warning "writing 3 bytes into a region of size 2" }
T (a4, s, 3, 3);    // { dg-warning "writing 3 bytes into a region of size 1" }
T (a4, s, 4, 3);    // { dg-warning "writing 3 bytes into a region of size 0" }

T (a4, s, 0, 4);
T (a4, s, 1, 4);    // { dg-warning "writing 4 bytes into a region of size 3" }
T (a4, s, 2, 4);    // { dg-warning "writing 4 bytes into a region of size 2" }
T (a4, s, 3, 4);    // { dg-warning "writing 4 bytes into a region of size 1" }
T (a4, s, 4, 4);    // { dg-warning "writing 4 bytes into a region of size 0" }

T (a7, s, 3, 3);
T (a7, s, 4, 3);
T (a7, s, 5, 3);    // { dg-warning "writing 3 bytes into a region of size 2" }
T (a7, s, 6, 3);    // { dg-warning "writing 3 bytes into a region of size 1" }
T (a7, s, 7, 3);    // { dg-warning "writing 3 bytes into a region of size 0" }

T (a7, s, 3, 4);
T (a7, s, 4, 4);    // { dg-warning "writing 4 bytes into a region of size 3" }
T (a7, s, 5, 4);    // { dg-warning "writing 4 bytes into a region of size 2" }
T (a7, s, 6, 4);    // { dg-warning "writing 4 bytes into a region of size 1" }
T (a7, s, 7, 4);    // { dg-warning "writing 4 bytes into a region of size 0" }

T (a7, s, 1, 6);
T (a7, s, 2, 6);    // { dg-warning "writing 6 bytes into a region of size 5" }
T (a7, s, 3, 6);    // { dg-warning "writing 6 bytes into a region of size 4" }
T (a7, s, 4, 6);    // { dg-warning "writing 6 bytes into a region of size 3" }
T (a7, s, 5, 6);    // { dg-warning "writing 6 bytes into a region of size 2" }
T (a7, s, 6, 6);    // { dg-warning "writing 6 bytes into a region of size 1" }
T (a7, s, 7, 6);    // { dg-warning "writing 6 bytes into a region of size 0" }

T (a8, s, 1, 7);
T (a8, s, 2, 7);    // { dg-warning "writing 7 bytes into a region of size 6" }
T (a8, s, 3, 7);    // { dg-warning "writing 7 bytes into a region of size 5" }
T (a8, s, 4, 7);    // { dg-warning "writing 7 bytes into a region of size 4" }
T (a8, s, 5, 7);    // { dg-warning "writing 7 bytes into a region of size 3" }
T (a8, s, 6, 7);    // { dg-warning "writing 7 bytes into a region of size 2" }
T (a8, s, 7, 7);    // { dg-warning "writing 7 bytes into a region of size 1" }
T (a8, s, 8, 7);    // { dg-warning "writing 7 bytes into a region of size 0" }

#undef T
#define T(dst, src, off, n)					\
  NOIPA void CAT (test_on_line_, __LINE__) (const void *s)	\
  {								\
    char *d = dst + off;					\
    __builtin_memcpy (d, src, n);				\
  } typedef void dummy_type

T (a4, s, 0, 1);
T (a4, s, 1, 1);
T (a4, s, 2, 1);
T (a4, s, 3, 1);
T (a4, s, 4, 1);    // { dg-warning "writing 1 byte into a region of size 0" }

T (a4, s, 0, 2);
T (a4, s, 1, 2);
T (a4, s, 2, 2);
T (a4, s, 3, 2);    // { dg-warning "writing 2 bytes into a region of size 1" }
T (a4, s, 4, 2);    // { dg-warning "writing 2 bytes into a region of size 0" }

T (a4, s, 0, 3);
T (a4, s, 1, 3);
T (a4, s, 2, 3);    // { dg-warning "writing 3 bytes into a region of size 2" }
T (a4, s, 3, 3);    // { dg-warning "writing 3 bytes into a region of size 1" }
T (a4, s, 4, 3);    // { dg-warning "writing 3 bytes into a region of size 0" }

T (a4, s, 0, 4);
T (a4, s, 1, 4);    // { dg-warning "writing 4 bytes into a region of size 3" }
T (a4, s, 2, 4);    // { dg-warning "writing 4 bytes into a region of size 2" }
T (a4, s, 3, 4);    // { dg-warning "writing 4 bytes into a region of size 1" }
T (a4, s, 4, 4);    // { dg-warning "writing 4 bytes into a region of size 0" }

T (a7, s, 3, 3);
T (a7, s, 4, 3);
T (a7, s, 5, 3);    // { dg-warning "writing 3 bytes into a region of size 2" }
T (a7, s, 6, 3);    // { dg-warning "writing 3 bytes into a region of size 1" }
T (a7, s, 7, 3);    // { dg-warning "writing 3 bytes into a region of size 0" }

T (a7, s, 3, 4);
T (a7, s, 4, 4);    // { dg-warning "writing 4 bytes into a region of size 3" }
T (a7, s, 5, 4);    // { dg-warning "writing 4 bytes into a region of size 2" }
T (a7, s, 6, 4);    // { dg-warning "writing 4 bytes into a region of size 1" }
T (a7, s, 7, 4);    // { dg-warning "writing 4 bytes into a region of size 0" }

T (a7, s, 1, 6);
T (a7, s, 2, 6);    // { dg-warning "writing 6 bytes into a region of size 5" }
T (a7, s, 3, 6);    // { dg-warning "writing 6 bytes into a region of size 4" }
T (a7, s, 4, 6);    // { dg-warning "writing 6 bytes into a region of size 3" }
T (a7, s, 5, 6);    // { dg-warning "writing 6 bytes into a region of size 2" }
T (a7, s, 6, 6);    // { dg-warning "writing 6 bytes into a region of size 1" }
T (a7, s, 7, 6);    // { dg-warning "writing 6 bytes into a region of size 0" }

#undef T
#define T(dst, src, init, off, n)			\
  NOIPA void CAT (test_on_line_, __LINE__) (void)	\
  {							\
    __builtin_strcpy (src, init);			\
    char *d = dst + off;				\
    __builtin_memcpy (d, src, n);			\
  } typedef void dummy_type


T (a6, b6, S4, 0, 4);
T (a6, b6, S4, 1, 4);
T (a6, b6, S4, 2, 4);
T (a6, b6, S4, 3, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a6, b6, S4, 4, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a6, b6, S4, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a6, b6, S4, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a7, b7, S4, 0, 4);
T (a7, b7, S4, 1, 4);
T (a7, b7, S4, 2, 4);
T (a7, b7, S4, 3, 4);
T (a7, b7, S4, 4, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a7, b7, S4, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a7, b7, S4, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a7, b7, S4, 7, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a8, b4, S3, 0, 4);
T (a8, b4, S3, 1, 4);
T (a8, b4, S3, 2, 4);
T (a8, b4, S3, 3, 4);
T (a8, b4, S3, 4, 4);
T (a8, b4, S3, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a8, b4, S3, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a8, b4, S3, 7, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a8, b4, S3, 8, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a8, b8, S3, 0, 4);
T (a8, b8, S3, 1, 4);
T (a8, b8, S3, 2, 4);
T (a8, b8, S3, 3, 4);
T (a8, b8, S3, 4, 4);
T (a8, b8, S3, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a8, b8, S3, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a8, b8, S3, 7, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a8, b8, S3, 8, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a8, b8, S4, 0, 4);
T (a8, b8, S4, 1, 4);
T (a8, b8, S4, 2, 4);
T (a8, b8, S4, 3, 4);
T (a8, b8, S4, 4, 4);
T (a8, b8, S4, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a8, b8, S4, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a8, b8, S4, 7, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a8, b8, S4, 8, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a8, b8, S4, 0, 5);
T (a8, b8, S4, 1, 5);
T (a8, b8, S4, 2, 5);
T (a8, b8, S4, 3, 5);
T (a8, b8, S4, 4, 5);   // { dg-warning "writing 5 bytes into a region of size 4" } */
T (a8, b8, S4, 5, 5);   // { dg-warning "writing 5 bytes into a region of size 3" } */
T (a8, b8, S4, 6, 5);   // { dg-warning "writing 5 bytes into a region of size 2" } */
T (a8, b8, S4, 7, 5);   // { dg-warning "writing 5 bytes into a region of size 1" } */
T (a8, b8, S4, 8, 5);   // { dg-warning "writing 5 bytes into a region of size 0" } */

T (a8, b8, S4, 0, 6);
T (a8, b8, S4, 1, 6);
T (a8, b8, S4, 2, 6);
T (a8, b8, S4, 3, 6);   // { dg-warning "writing 6 bytes into a region of size 5" } */
T (a8, b8, S4, 4, 6);   // { dg-warning "writing 6 bytes into a region of size 4" } */
T (a8, b8, S4, 5, 6);   // { dg-warning "writing 6 bytes into a region of size 3" } */
T (a8, b8, S4, 6, 6);   // { dg-warning "writing 6 bytes into a region of size 2" } */
T (a8, b8, S4, 7, 6);   // { dg-warning "writing 6 bytes into a region of size 1" } */
T (a8, b8, S4, 8, 6);   // { dg-warning "writing 6 bytes into a region of size 0" } */


#undef T
#define T(dst, init, off, n)				\
  NOIPA void CAT (test_on_line_, __LINE__) (char *src)	\
  {							\
    __builtin_strcpy (src, init);			\
    char *d = dst + off;				\
    __builtin_memcpy (d, src, n);			\
  } typedef void dummy_type

T (a6, S4, 0, 4);
T (a6, S4, 1, 4);
T (a6, S4, 2, 4);
T (a6, S4, 3, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a6, S4, 4, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a6, S4, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a6, S4, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a7, S4, 0, 4);
T (a7, S4, 1, 4);
T (a7, S4, 2, 4);
T (a7, S4, 3, 4);
T (a7, S4, 4, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a7, S4, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a7, S4, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a7, S4, 7, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */

T (a8, S3, 0, 4);
T (a8, S3, 1, 4);
T (a8, S3, 2, 4);
T (a8, S3, 3, 4);
T (a8, S3, 4, 4);
T (a8, S3, 5, 4);   // { dg-warning "writing 4 bytes into a region of size 3" } */
T (a8, S3, 6, 4);   // { dg-warning "writing 4 bytes into a region of size 2" } */
T (a8, S3, 7, 4);   // { dg-warning "writing 4 bytes into a region of size 1" } */
T (a8, S3, 8, 4);   // { dg-warning "writing 4 bytes into a region of size 0" } */
