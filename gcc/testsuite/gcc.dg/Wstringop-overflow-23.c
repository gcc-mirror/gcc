/* PR middle-end/83859 - attribute to establish relation between parameters
   for buffer and its size
   Test to verify that with optimization enabled, -Wstringop-overflow
   warnings are issued for calls to user-defined functions with attribute
   access and with non-constant out-of-bounds arguments.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#include "range.h"

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)

#define RDONLY(...)  __attribute__ ((access (read_only, __VA_ARGS__)))
#define WRONLY(...)  __attribute__ ((access (write_only, __VA_ARGS__)))
#define RDWR(...)  __attribute__ ((access (read_write, __VA_ARGS__)))

typedef __INT32_TYPE__ int32_t;

/* Exercise null pointer detection.  */

RDONLY (2, 1) void
rd2_1 (int, const void*);       // { dg-message "in a call to function 'rd2_1' declared with attribute 'read_only \\\(2, 1\\\)" }

void test_rd2_1 (void)
{
  {
    void *null = 0;
    void *p = &null;

    rd2_1 (0, null);
    rd2_1 (1, p);
  }

  {
    void *null = 0;
    rd2_1 (1, null);            // { dg-warning "argument 2 is null but the corresponding size argument 1 value is 1" }
  }

  {
    void *null = 0;
    rd2_1 (SR (1, 2), null);    // { dg-warning "argument 2 is null but the corresponding size argument 1 range is \\\[1, 2]" }
  }
}

WRONLY (3, 1) void
wr3_1 (int, int, void*);        // { dg-message "in a call to function 'wr3_1' declared with attribute 'write_only \\\(3, 1\\\)" }

void test_wr3_1 (void)
{
  {
    void *null = 0;
    void *p = &null;

    wr3_1 (SR (0, 1), 0, null);
    wr3_1 (SR (1, 1), 0, p);
  }

  void *null = 0;

  wr3_1 (SR (1, 2), 1, null);   // { dg-warning "argument 3 is null but the corresponding size argument 1 range is \\\[1, 2]" }
}


WRONLY (2, 1) void
wr2_1 (int, void*);

void test_wrd2_1 (int n)
{
  wr2_1 (0, 0);
  wr2_1 (SR (-1, 1), 0);
  wr2_1 (SR (0, 1), 0);
  wr2_1 (SR (1, 2), 0);         // { dg-warning "argument 2 is null but the corresponding size argument 1 range is \\\[1, 2]" }

  /* This should probably be diagnosed but to avoid false positives
     caused by jump threading and such it would have to be done
     earlier than it is now.  */
  wr2_1 (n, 0);                 // { dg-warning "argument 2 is null" "unimplemented" { xfail *-*-* } }
}


/* Exercise pointer to an incomplete type other than void.  */

struct Incomplete;
extern struct Incomplete inc;

extern char ax[];

WRONLY (1, 2) void
wr1_2_inc (struct Incomplete*, unsigned);

void test_wr1_2_inc (struct Incomplete *pinc, unsigned n)
{
  wr1_2_inc (0, 0);
  wr1_2_inc (0, 1);         // { dg-warning "argument 1 is null but the corresponding size argument 2 value is 1" }

  wr1_2_inc (pinc, 1);
  wr1_2_inc (&inc, 1);

  wr1_2_inc (pinc, 123);
  wr1_2_inc (&inc, 456);

  char a3[3];
  pinc = (struct Incomplete*)a3;
  wr1_2_inc (pinc, SR (3, 4));
  wr1_2_inc (pinc, SR (4, 5));
  // { dg-warning "'wr1_2_inc' writing between 4 and 5 bytes into a region of size 3" "small buffer cast to incomplete" { target *-*-* } .-1 }

  pinc = (struct Incomplete*)ax;
  wr1_2_inc (pinc, SR (123, 456));

  char vla[n];
  pinc = (struct Incomplete*)vla;
  wr1_2_inc (pinc, SR (345, 456));
}


RDONLY (1, 3) WRONLY (2, 4) void
rd1_3_wr2_4 (const void*, void*, int, int);

void test_rd1_3_wr2_4 (const void *s, void *d, int n1, int n2)
{
  rd1_3_wr2_4 (s, d, 1, 2);
  rd1_3_wr2_4 (s, d, 123, 456);
  rd1_3_wr2_4 (s, d, INT_MAX, INT_MAX);
  rd1_3_wr2_4 (s, d, -1, 2);    // { dg-warning "argument 3 value -1 is negative" }

  const int ir_min_m1 = SR (INT_MIN, -1);
  rd1_3_wr2_4 (s, d, ir_min_m1, 2);   // { dg-warning "argument 3 range \\\[-\[0-9\]+, -1] is negative" }

  rd1_3_wr2_4 (s, d, SR (-1, 0), 2);
  rd1_3_wr2_4 (s, d, SR (INT_MIN, INT_MAX), 2);

  rd1_3_wr2_4 (s, d, n1, n2);


  const char s11[11] = "0123456789";

  rd1_3_wr2_4 (s11, d, 11, n2);
  rd1_3_wr2_4 (s11, d, 12, n2);   // { dg-warning "'rd1_3_wr2_4' reading 12 bytes from a region of size 11" }

  rd1_3_wr2_4 (s11, d, SR (0, 11), n2);
  rd1_3_wr2_4 (s11, d, SR (0, 12), n2);
  rd1_3_wr2_4 (s11, d, SR (11, 12), n2);
  rd1_3_wr2_4 (s11, d, SR (11, INT_MAX), n2);
  rd1_3_wr2_4 (s11, d, SR (12, 13), n2);  // { dg-warning "'rd1_3_wr2_4' reading between 12 and 13 bytes from a region of size 11" }

  char d4[4];
  rd1_3_wr2_4 (s, d4, n1, 4);
  rd1_3_wr2_4 (s, d4, n1, 5);     // { dg-warning "'rd1_3_wr2_4' writing 5 bytes into a region of size 4" }

  rd1_3_wr2_4 (s11, d4, SR (12, 13), SR (5, 6));
  // { dg-warning "'rd1_3_wr2_4' reading between 12 and 13 bytes from a region of size 11" "read" { target *-*-* } .-1 }
  // { dg-warning "'rd1_3_wr2_4' writing between 5 and 6 bytes into a region of size 4" "read" { target *-*-* } .-2 }
}


/* Verify that function pointers are handled.  */

RDONLY (1) void (*pfrd1)(const void*, const void*);

void test_pfrd1 (void)
{
  pfrd1 ("" + SR (0, 9), "" + SR (1, 9));
  pfrd1 ("" + SR (1, 2), "");   // { dg-warning "reading 1 byte from a region of size 0" }
}


WRONLY (4, 3) void (*pfwr4_3)(int, const char*, int, int*);

void test_pfwr4_3 (void)
{
  int32_t i;
  pfwr4_3 (3, "", 0, &i + SR (0, 9));
  pfwr4_3 (5, "", 1, &i + SR (1, 2));   // { dg-warning "writing 4 bytes into a region of size 0" }
}
