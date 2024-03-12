/* Test BTF generation of struct type.

   Two BTF_KIND_STRUCT records are expected.
   - struct foo with 3 members
   - struct data with 2 members  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x4000003\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x4000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times " btm_type: \\(BTF_KIND_INT" 3 } } */
/* { dg-final { scan-assembler-times " btm_type: \\(BTF_KIND_ARRAY" 1 } } */
/* { dg-final { scan-assembler-times " btm_type: \\(BTF_KIND_STRUCT" 1 } } */

struct foo
{
  int after;
  int before;
  struct {
    unsigned short n_valid;
    int set[10];
  } data;
} my_foo;
