/* PR c/53037.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int foo1 __attribute__((warn_if_not_aligned(8))); /* { dg-error "5:'warn_if_not_aligned' may not be specified for 'foo1'" } */

__attribute__((warn_if_not_aligned(8)))
void
foo2 (void) /* { dg-error "1:'warn_if_not_aligned' may not be specified for 'void foo2\\(\\)'" } */
{
}

struct foo3
{
  int i : 2 __attribute__((warn_if_not_aligned(8))); /* { dg-error "7:'warn_if_not_aligned' may not be specified for 'i'" } */
};

typedef unsigned int __u32
  __attribute__((aligned(4),warn_if_not_aligned(8)));

struct foo4
{
  __u32 i : 2; /* { dg-error "9:cannot declare bit-field 'i' with 'warn_if_not_aligned' type" } */
};
