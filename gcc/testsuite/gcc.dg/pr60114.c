/* PR c/60114 */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */
/* { dg-require-effective-target int32plus } */

struct S { int n, u[2]; };
const signed char z[] = {
  [0] = 0x100, /* { dg-warning "-Woverflow" } */
  [2] = 0x101, /* { dg-warning "-Woverflow" } */
};
int A[] = {
            0, 0x80000000, /* { dg-warning "16:-Wsign-conversion" } */
            0xA, 0x80000000, /* { dg-warning "18:-Wsign-conversion" } */
            0xA, 0xA, 0x80000000 /* { dg-warning "23:-Wsign-conversion" } */
          };
int *p = (int []) { 0x80000000 }; /* { dg-warning "21:-Wsign-conversion" } */
union { int k; } u = { .k = 0x80000000 }; /* { dg-warning "29:-Wsign-conversion" } */
typedef int H[];
void
foo (void)
{
  signed char a[][3] = { { 0x100, /* { dg-warning "28:-Woverflow" } */
                    1, 0x100 }, /* { dg-warning "24:-Woverflow" } */
                  { '\0', 0x100, '\0' } /* { dg-warning "27:-Woverflow" } */
                };
  (const signed char []) { 0x100 }; /* { dg-warning "28:-Woverflow" } */
  (const float []) { 1e0, 1e1, 1e100 }; /* { dg-warning "32:conversion" } */
  struct S s1 = { 0x80000000 }; /* { dg-warning "19:-Wsign-conversion" } */
  struct S s2 = { .n = 0x80000000 }; /* { dg-warning "24:-Wsign-conversion" } */
  struct S s3 = { .u[1] = 0x80000000 }; /* { dg-warning "27:-Wsign-conversion" } */
  H h = { 1, 2, 0x80000000 }; /* { dg-warning "17:-Wsign-conversion" } */
}
