/* PR tree-optimization/28887 */
/* { dg-do compile } */
/* { dg-options "-O2 -fprefetch-loop-arrays -w" } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=i686 -msse -w" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

__extension__ typedef __SIZE_TYPE__ size_t;

struct re_pattern_buffer
{
  size_t re_nsub;
};

typedef enum
{
  start_memory,
} re_opcode_t;

typedef union
{
  struct
  {
    unsigned matched_something:1;
  } bits;
} byte_register_info_type;

void byte_re_match_2_internal (struct re_pattern_buffer *bufp)
{
  int mcnt;
  size_t num_regs = bufp->re_nsub + 1;
  byte_register_info_type *reg_info;
  for (mcnt = 1; (unsigned) mcnt < num_regs; mcnt++)
    {
      ((reg_info[mcnt]).bits.matched_something) = 0;
    }
}

