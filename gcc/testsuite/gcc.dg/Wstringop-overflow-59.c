/* PR middle-end/92936 - missing warning on a past-the-end store to a PHI
   Exercise warnings for writing into one of two or more allocated objects.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#include "range.h"

#define INT_MAX __INT_MAX__

extern void* malloc (size_t);
extern void* memset (void*, int, size_t);
#define memset(d, c, n) sink (memset (d, c, n))

void sink (int, ...);
#define sink(...) sink (0, __VA_ARGS__)

volatile int cond1, cond2, x;

#define CHOOSE_MALLOC_2(n1, n2)			\
  (cond1 ? malloc (n1) : malloc (n2))
#define CHOOSE_MALLOC_3(n1, n2, n3)					\
  (cond1 < 0 ? malloc (n1) : 0 < cond1 ? malloc (n2) : malloc (n3))


void memset_malloc_2 (void)
{
  {
    char *p0_1 = CHOOSE_MALLOC_2 (0, 1);

    memset (p0_1, 0, 0);
    /* Writing more than the smallest destination should trigger a "may
       write" warning if the access is unconditionally reachable from
       the block where the pointer to either object is assigned.  */
    memset (p0_1, 0, 1);
    memset (p0_1, 0, 2);      // { dg-warning "memset' writing 2 bytes into a region of size 1 " }
    memset (p0_1, 0, 9);      // { dg-warning "memset' writing 9 bytes into a region of size 1 " }
  }

  {
    char *p0_x = CHOOSE_MALLOC_2 (0, x);

    memset (p0_x, 0, 0);
    memset (p0_x, 0, 1);
    memset (p0_x, 0, 2);
    memset (p0_x, 0, 12345);
  }

  {
    char *px_x = CHOOSE_MALLOC_2 (x, x);

    memset (px_x, 0, 0);
    memset (px_x, 0, 1);
    memset (px_x, 0, 2);
    memset (px_x, 0, 12345);
  }

  {
    char *p3_5 = CHOOSE_MALLOC_2 (3, 5);

    memset (p3_5, 0, 1);
    memset (p3_5, 0, 3);
    memset (p3_5, 0, 4);
    memset (p3_5, 0, 5);
    memset (p3_5, 0, 6);      // { dg-warning "memset' writing 6 bytes into a region of size 5 " }
  }

  {
    char *p5_3 = CHOOSE_MALLOC_2 (5, 3);

    memset (p5_3, 0, 3);
    memset (p5_3, 0, 4);
    memset (p5_3, 0, 5);
    memset (p5_3, 0, 6);      // { dg-warning "memset' writing 6 bytes into a region of size 5 " }
  }

  {
    char *px_3 = CHOOSE_MALLOC_2 (x, 3);

    memset (px_3, 0, 1);
    memset (px_3, 0, 3);
    memset (px_3, 0, 4);
    memset (px_3, 0, 1234);
  }

  {
    char *p5_x = CHOOSE_MALLOC_2 (5, x);

    memset (p5_x, 0, 1);
    memset (p5_x, 0, 5);
    memset (p5_x, 0, 6);
    memset (p5_x, 0, 1234);
  }

}


void memset_malloc_3 (void)
{
  {
    char *p0_1_2 = CHOOSE_MALLOC_3 (0, 1, 2);
    memset (p0_1_2, 0, 0);
    memset (p0_1_2, 0, 1);
    memset (p0_1_2, 0, 2);
    memset (p0_1_2, 0, 3);    // { dg-warning "memset' writing 3 bytes into a region of size 2 " }
    memset (p0_1_2, 0, 9);    // { dg-warning "memset' writing 9 bytes into a region of size 2 " }
  }

  {
    char *p0_2_x = CHOOSE_MALLOC_3 (0, 2, x);

    memset (p0_2_x, 0, 0);
    memset (p0_2_x, 0, 1);
    memset (p0_2_x, 0, 3);
    memset (p0_2_x, 0, 9);
  }

  {
    char *p3_4_5 = CHOOSE_MALLOC_3 (3, 4, 5);

    memset (p3_4_5, 0, 3);
    memset (p3_4_5, 0, 4);
    memset (p3_4_5, 0, 5);
    memset (p3_4_5, 0, 6);    // { dg-warning "memset' writing 6 bytes into a region of size 5 " }
  }

  {
    char *p5_3_4 = CHOOSE_MALLOC_3 (5, 3, 4);

    memset (p5_3_4, 0, 3);
    memset (p5_3_4, 0, 4);
    memset (p5_3_4, 0, 5);
    memset (p5_3_4, 0, 6);    // { dg-warning "memset' writing 6 bytes into a region of size 5 " }
  }

  {
    char *p9_8_7 = CHOOSE_MALLOC_3 (9, 8, 7);

    memset (p9_8_7, 0, 7);
    memset (p9_8_7, 0, 8);
    memset (p9_8_7, 0, 9);
    memset (p9_8_7, 0, 10);   // { dg-warning "memset' writing 10 bytes into a region of size 9 " }
  }
}


/* Verify conditionally writing into one of two objects with the same
   size.  */

void memset_malloc_2_same_size (int i)
{
  {
    char a4_1[4], a4_2[4];
    char *p4 = cond1 ? a4_1 : a4_2;

    memset (p4, 0, 1);
    memset (p4, 0, 2);
    memset (p4, 0, 3);
    memset (p4, 0, 4);
    memset (p4, 0, 5);        // { dg-warning "memset' writing 5 bytes into a region of size 4" }
  }

  {
    char a4_1[4];             // { dg-message "destination object 'a4_1" "note" }
    char a4_2[4];             // { dg-message "destination object 'a4_2" "note" }
    char *p4 = cond1 ? a4_1 : a4_2;
    char *p4_i = p4 + i;

    memset (p4_i, 0, 5);      // { dg-warning "memset' writing 5 bytes into a region of size 4" }
  }

  {
    if (i < 1)
      i = 1;

    char a4_1[4];             // { dg-message "at offset \\\[1, 4] into destination object 'a4_1" "note" }
    char a4_2[4];             // { dg-message "at offset \\\[1, 4] into destination object 'a4_2" "note" }
    char *p4 = cond1 ? a4_1 : a4_2;
    char *p4_i = p4 + i;

    memset (p4_i, 0, 3);
    memset (p4_i, 0, 4);      // { dg-warning "memset' writing 4 bytes into a region of size 3 " }
  }
}


void memset_malloc_2_off (void)
{
  int i1 = SR (1, INT_MAX);
  int i2 = SR (2, INT_MAX);

  {
    char a5[5];               // { dg-warning "at offset [1, 5] into destination object 'a5'
    char a7[7];               // { dg-warning "at offset [2, 7] into destination object 'a7'
    char *p5_p1 = a5 + i1;
    char *p7_p2 = a7 + i2;
    char *p5_7 = cond1 ? p5_p1 : p7_p2;

    memset (p5_7, 0, 1);
    memset (p5_7, 0, 2);
    memset (p5_7, 0, 3);
    memset (p5_7, 0, 4);
    memset (p5_7, 0, 5);
    memset (p5_7, 0, 6);      // { dg-warning "memset' writing 6 bytes into a region of size 5 " }
  }

  int i3 = SR (3, INT_MAX);

  {
    char a5[5];
    // { dg-message "at offset \\\[3, 5] into destination object 'a5'" "note" { target *-*-* } .-1 }
    // { dg-message "at offset \\\[2, 5] into destination object 'a5'" "note" { target *-*-* } .-2 }
    // { dg-message "at offset \\\[1, 5] into destination object 'a5'" "note" { target *-*-* } .-3 }
    // { dg-message ": destination object 'a5'" "note" { target *-*-* } .-4 }
    char a9[9];
    // { dg-message "at offset \\\[4, 9] into destination object 'a9'" "note" { target *-*-* } .-1 }
    // { dg-message "at offset \\\[3, 9] into destination object 'a9'" "note" { target *-*-* } .-2 }
    // { dg-message "at offset \\\[2, 9] into destination object 'a9'" "note" { target *-*-* } .-3 }
    // { dg-message ": destination object 'a9'" "note" { target *-*-* } .-4 }
    char *p5_p2 = a5 + i2;    // 3 bytes left
    char *p9_p3 = a9 + i3;    // 6 bytes left
    char *p =
      cond1 ? p5_p2 : p9_p3;  // [3 - 6] bytes left
    char *q = p + i1;         // [2 - 5] bytes left

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);
    memset (q, 0, 5);
    memset (q, 0, 6);         // { dg-warning "memset' writing 6 bytes into a region of size 5" }

    --q;                      // [3 - 6] bytes left
    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);
    memset (q, 0, 5);
    memset (q, 0, 6);
    memset (q, 0, 7);         // { dg-warning "memset' writing 7 bytes into a region of size 6" }

    --q;                      // [4 - 7] bytes left
    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);
    memset (q, 0, 5);
    memset (q, 0, 6);
    memset (q, 0, 7);
    memset (q, 0, 8);         // { dg-warning "memset' writing 8 bytes into a region of size 7" }

    int m1_x = SR (-1, INT_MAX);
    int m2_x = SR (-2, INT_MAX);

    q += cond2 ? m1_x : m2_x;   // [5 - 9] bytes left

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);
    memset (q, 0, 5);
    memset (q, 0, 6);
    memset (q, 0, 7);
    memset (q, 0, 8);
    memset (q, 0, 9);
    memset (q, 0, 10);        // { dg-warning "memset' writing 10 bytes into a region of size 9" }
  }
}
