/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O3 -fpic -fplt -mtls-dialect=gnu" } */

typedef enum
{
  bfd_error_invalid_error_code
} bfd_error_type;
static thread_local bfd_error_type bfd_error;
extern int sections;
extern void *bfd_alloc_ret;
extern int bfd_alloc___o;
extern long bfd_alloc_size;

extern void _objalloc_alloc (int *, long);

bfd_error_type
bfd_get_error ()
{
  return bfd_error;
}

bool
s7_bfd_score_elf_late_size_sections ()
{
  for (; sections;)
    {
      if (bfd_alloc_size)
        {
          bfd_error_type error_tag;
          bfd_error = error_tag;
        }
      _objalloc_alloc (&bfd_alloc___o, 0);
      if (bfd_alloc_ret)
        {
          bfd_error_type error_tag;
          bfd_error = error_tag;
        }
    }
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 2 { target { ! ia32 } } } } */
