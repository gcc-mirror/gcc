/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu -fno-semantic-interposition -fstack-protector" } */

typedef enum
{
  bfd_error_invalid_error_code
} bfd_error_type;
thread_local bfd_error_type bfd_error;
int aout_16_write_syms___trans_tmp_1;
short aout_16_write_syms_g_0_0;
void xvec_0 (long, void *);

typedef struct
{
  int output_section;
} asection;

void bfd_asymbol_section ();

struct pdp11_external_nlist
{
  char e_desc[2];
  char e_type[1];
  char e_ovly[10];
} translate_to_native_sym_flags (struct pdp11_external_nlist *sym_pointer)
{
  asection *sec;
  sym_pointer->e_type[0] &= 5;
  bfd_asymbol_section ();
  if (sec == 0)
    {
      bfd_error_type error_tag;
      bfd_error = error_tag;
    }
  if (sec->output_section)
    {
      bfd_error_type error_tag;
      bfd_error = error_tag;
    }
}

bool
aout_16_write_syms (void *abfd)
{
  for (; aout_16_write_syms___trans_tmp_1;)
    {
      struct pdp11_external_nlist nsp;
      if (abfd)
        {
          xvec_0 (aout_16_write_syms_g_0_0, nsp.e_desc);
          nsp.e_ovly[0] = 0;
        }
      else
        nsp.e_type[0] = 0;
      translate_to_native_sym_flags (&nsp);
    }
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 2 { target { ! ia32 } } } } */
