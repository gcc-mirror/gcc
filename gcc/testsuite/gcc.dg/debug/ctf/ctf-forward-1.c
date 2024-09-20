/* CTF forward type is generated for forward declarations of types in C.
   
   Check that the ctf-kind of CTF_K_FOWARD type is CTF_K_STRUCT or CTF_K_UNION.
   For forward types, the compiler encodes the CTF kind in the ctt_type field.
   CTF_K_FORWARD is used as the CTF type as usual in the ctt_info.  */

/* Note - A value of 6 in "ctt_size or ctt_type" appears twice in this
   testcase. This might be misconstrued as  2 CTK_K_FORWARD records of struct
   type.  The second assembler tag is due to a ref type in a CVR CTF record.
   TBD - perhaps a more robust string pattern is needed.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x26000000\[\t \]+\[^\n\]*ctt_info" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x6\[\t \]+\[^\n\]*ctt_size or ctt_type" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x7\[\t \]+\[^\n\]*ctt_size or ctt_type" 2 } } */

typedef struct __locale_struct
{
  struct __locale_data *__locales[13]; /* forward struct type.  */

  const int *__ctype_toupper;
  const char *__names[13];
} *__locale_t;

typedef __locale_t locale_t;

locale_t loc;

typedef struct __inter_struct
{
  union __inter_data * __inters[13]; /* forward union type.  */

  const int * __ctype_kind;
} * __inter_t;

typedef __inter_t inter_t;

inter_t inter;
