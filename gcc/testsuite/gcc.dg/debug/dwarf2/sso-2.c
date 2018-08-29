/* { dg-do compile } */
/* { dg-options "-gdwarf-3 -dA" } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REVERSE_SSO __attribute__((scalar_storage_order("big-endian")));
#else
#define REVERSE_SSO __attribute__((scalar_storage_order("little-endian")));
#endif

struct reverse
{
  int i;
  short a[4];
} REVERSE_SSO;

struct native
{
  int i;
  short a[4];
};

struct reverse R;
struct native  N;

/* Verify that we have endianity on the common base type of 'i' and the
 *  element of 'a' in the first 2 structures.  */
/* { dg-final { scan-assembler-times " DW_AT_endianity" 2 } } */
/* { dg-final { scan-assembler-times "DIE \\(\[0-9a-z\]*\\) DW_TAG_base_type" 5 } } */
