/* Check that 128-bit struct's are represented as TImode values.  */
/* { dg-do compile { target int128 } } */
/* { dg-skip-if "different ABI" { x86_64-*-mingw* } } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

struct shared_ptr_struct
{
  unsigned long long phase:48;
  unsigned short thread:16;
  union
    {
      void *addr;
      unsigned long long pad;
    };
};
typedef struct shared_ptr_struct sptr_t;

void
copy_sptr (sptr_t *dest, sptr_t src)
{
  *dest = src;
}

/* { dg-final { scan-rtl-dump "\\\(set \\\(reg:TI \[0-9\]*" "expand" } } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
