/* Test BTF generation of forwards.

   Check that the KIND_FLAG (bit 31) of btt_info is set (1) for the forward to
   union, and not set (0) for forward to struct.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x87000000\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x7000000\[\t \]+\[^\n\]*btt_info" 1 } } */

typedef struct _fwd_st
{
  struct unk_struct_type *data[4];
} fwd_st_t;

fwd_st_t struct_container;

typedef struct _fwd_un
{
  union unk_union_type *options[4];
} fwd_un_t;

fwd_un_t union_container;
