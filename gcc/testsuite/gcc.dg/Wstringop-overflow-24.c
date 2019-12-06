/* PR middle-end/83859 - attribute to establish relation between parameters
   for buffer and its size
   Test to verify that -Wstringop-overflow warnings are issued even with
   no optimization for calls to user-defined functions with attribute
   access and with constant out-of-bounds arguments.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)

#define rdonly       __attribute__ ((access (read_only)))
#define RDONLY(...)  __attribute__ ((access (read_only, __VA_ARGS__)))
#define WRONLY(...)  __attribute__ ((access (write_only, __VA_ARGS__)))
#define RDWR(...)    __attribute__ ((access (read_write, __VA_ARGS__)))

typedef __INT32_TYPE__ int32_t;

extern const char s1[1], s2[2], s3[3];
extern char d1[1], d2[2], d3[3];

/* Exercise that null pointers are allowed in functions declared with
   the attribute without a size operand.  */

RDONLY (1) void
rd1_int (const int*);       // { dg-message "in a call to function 'rd1_int' declared with attribute 'read_only \\\(1\\\)'" }

void test_rd1_int (void)
{
  rd1_int (0);

  int32_t i = 0;
  rd1_int (&i);

  rd1_int ((int32_t*)s1);   // { dg-warning "reading 4 bytes from a region of size 1" }
}

/* Exercise null pointer detection in functions declared with
   the attribute and with non-zero size.  */

RDONLY (2, 1) void
rd2_1 (int, const void*);   // { dg-message "in a call to function 'rd2_1' declared with attribute 'read_only \\\(2, 1\\\)" }

void test_rd2_1 (void)
{
  rd2_1 (0, 0);
  rd2_1 (1, "");
  rd2_1 (1, 0);             // { dg-warning "argument 2 is null but the corresponding size argument 1 value is 1" }
}

WRONLY (3, 1) void
wr3_1 (int, int, void*);    // { dg-message "in a call to function 'wr3_1' declared with attribute 'write_only \\\(3, 1\\\)" }

void test_wr3_1 (void)
{
  wr3_1 (0, 0, 0);
  wr3_1 (1, 0, d1);
  wr3_1 (2, 1, 0);          // { dg-warning "argument 3 is null but the corresponding size argument 1 value is 2" }
}


/* Exercise pointer to an incomplete type other than void.  */

struct Incomplete;
extern struct Incomplete inc;

RDONLY (1) void
rd_inc (const struct Incomplete*);

void test_rd_inc (const struct Incomplete *pinc)
{
  rd_inc (0);

  rd_inc (pinc);
  rd_inc ((const struct Incomplete*)s1);

  rd_inc ((const struct Incomplete*)&s1[1]);
  // { dg-warning "'rd_inc' reading 1 byte from a region of size 0" "past-the-end pointer" { target *-*-* } .-1 }
}

RDONLY (1, 2) void
rd1_2_inc (const struct Incomplete*, unsigned);

void test_rd1_2_inc (const struct Incomplete *pinc)
{
  rd1_2_inc (0, 0);
  rd1_2_inc (0, 1);         // { dg-warning "argument 1 is null but the corresponding size argument 2 value is 1" }

  rd1_2_inc (pinc, 1);
  rd1_2_inc (&inc, 1);

  rd1_2_inc (pinc, 123);
  rd1_2_inc (&inc, 456);

  rd1_2_inc ((const struct Incomplete*)s3, 4);
  // { dg-warning "'rd1_2_inc' reading 4 bytes from a region of size 3" "small buffer cast to incomplete" { target *-*-* } .-1 }
}


/* Verify the handling of two attributes sharing the same size operand .  */

RDONLY (1, 3) WRONLY (2, 3) void
rd1_3_wr2_3 (const void*, void*, int);

void test_rd1_3_wr2_3 (void)
{
  rd1_3_wr2_3 (s1, d1, 0);
  rd1_3_wr2_3 (s1, d1, 1);

  rd1_3_wr2_3 (s1, d1, 2);
  // { dg-warning "'rd1_3_wr2_3' reading 2 bytes from a region of size 1" "read" { target *-*-* } .-1 }
  // { dg-warning "'rd1_3_wr2_3' writing 2 bytes into a region of size 1" "write" { target *-*-* } .-2 }

  rd1_3_wr2_3 (s1, d2, 2);
  // { dg-warning "'rd1_3_wr2_3' reading 2 bytes from a region of size 1" "read" { target *-*-* } .-1 }

  rd1_3_wr2_3 (s2, d1, 2);
  // { dg-warning "'rd1_3_wr2_3' writing 2 bytes into a region of size 1" "write" { target *-*-* } .-1 }
}


/* Verify the handling of multiple attributes of the same kind with
   out-of-order operands.  */

RDONLY (1, 6) RDONLY (2, 5) RDONLY (3, 4) void
rd1_6_2_5_3_4 (const void *s1, const void *s2, const void *s3,
	       int         n3, int         n2, int         n1);

void test_rd1_6_2_5_3_4 (void)
{
  rd1_6_2_5_3_4 (s1, s2, s3, 4, 2, 1);   // { dg-warning "reading 4 bytes from a region of size 3" }
  rd1_6_2_5_3_4 (s1, s2, s3, 3, 5, 1);   // { dg-warning "reading 5 bytes from a region of size 2" }
  rd1_6_2_5_3_4 (s1, s2, s3, 3, 2, 6);   // { dg-warning "reading 6 bytes from a region of size 1" }
}


/* Verify the handling of multiple attributes of different kinds with
   out-of-order operands.  */

RDONLY (1, 6) WRONLY (2, 5) RDONLY (3, 4) void
rd1_6_wr2_5_rd3_4 (const void *s1, void *d2, const void *s3,
		   int         n3, int   n2, int         n1);

void test_rd1_6_wr2_5_rd3_4 (void)
{
  rd1_6_wr2_5_rd3_4 (s1, d2, s3, 7, 2, 1);   // { dg-warning "reading 7 bytes from a region of size 3" }
  rd1_6_wr2_5_rd3_4 (s1, d2, s3, 3, 8, 1);   // { dg-warning "writing 8 bytes into a region of size 2" }
  rd1_6_wr2_5_rd3_4 (s1, d2, s3, 3, 2, 9);   // { dg-warning "reading 9 bytes from a region of size 1" }
}


RDONLY (6, 1) WRONLY (5, 2) RDWR (4, 3) void
rd6_1_wr5_2_rd4_3 (int   n1, int   n2, int         n3,
		   void *d3, void *d2, const void *s1);

void test_rd6_1_wr5_2_rd4_3 (void)
{
  rd6_1_wr5_2_rd4_3 (7, 2, 1, d1, d2, s3);   // { dg-warning "reading 7 bytes from a region of size 3" }
  rd6_1_wr5_2_rd4_3 (3, 8, 1, d1, d2, s3);   // { dg-warning "writing 8 bytes into a region of size 2" }
  rd6_1_wr5_2_rd4_3 (3, 2, 9, d1, d2, s3);   // { dg-warning "writing 9 bytes into a region of size 1" }
}


RDONLY (1, 3) WRONLY (2, 4) void
rd1_3_wr2_4 (const void*, void*, int, int);

void test_rd1_3_wr2_4 (const void *s, void *d, int n1, int n2)
{
  rd1_3_wr2_4 (s, d, 1, 2);
  rd1_3_wr2_4 (s, d, 123, 456);
  rd1_3_wr2_4 (s, d, INT_MAX, INT_MAX);
  rd1_3_wr2_4 (s, d, -1, 2);    // { dg-warning "argument 3 value -1 is negative" }

  const char s11[11] = "0123456789";

  rd1_3_wr2_4 (s11, d, 11, n2);
  rd1_3_wr2_4 (s11, d, 12, n2);   // { dg-warning "'rd1_3_wr2_4' reading 12 bytes from a region of size 11" }
}


/* Verify that function pointers are handled.  */

RDONLY (1) void (*pfrd1)(const void*, const void*);

void test_pfrd1 (void)
{
  pfrd1 (0, 0);
  pfrd1 ("", "");

  pfrd1 ("", "" + 1);
  pfrd1 ("" + 1, "");   // { dg-warning "reading 1 byte from a region of size 0" }
}


WRONLY (4, 3) void (*pfwr4_3)(int, const char*, int, int*);

void test_pfwr4_3 (void)
{
  pfwr4_3 (0, 0, 0, 0);

  int32_t i;
  pfwr4_3 (3, "", 0, &i + 1);
  pfwr4_3 (5, "", 1, &i + 1);   // { dg-warning "writing 4 bytes into a region of size 0" }
}
