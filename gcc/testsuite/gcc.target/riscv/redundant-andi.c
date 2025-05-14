/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */


typedef struct sv SV;

typedef struct magic MAGIC;
typedef short I16;
typedef unsigned short U16;
typedef int I32;
typedef unsigned int U32;
struct sv
{
  U32 sv_refcnt;
  U32 sv_flags;
};
struct magic
{
  U16 mg_private;
};
extern SV **PL_psig_ptr;
int
Perl_magic_setsig (SV *sv, MAGIC *mg, const char *s)
{
  I32 i;
  i = (I16) mg->mg_private;
  if (sv)
    {
      PL_psig_ptr[i] = (++((sv)->sv_refcnt), ((SV *) ((void *) (sv))));
      ((sv)->sv_flags &= ~0x00080000);
    }
  else
    {
      PL_psig_ptr[i] = ((void *) 0);
    }
  return 0;
}

/* { dg-final { scan-assembler-not "andi\t" } } */

