/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void frob (void);
typedef struct av AV;
typedef unsigned int U32;
struct av
{
  void *dummy;
  U32 sv_refcnt;
  U32 sv_flags;
};
void
Perl_save_ary (AV *const oav)
{
  AV *av;
  unsigned int x1 = oav->sv_flags;
  unsigned int x2 = x1 & 3221225472;
  if (x2 == 2147483648)
    frob ();
}

/* { dg-final { scan-assembler-not "andi\t" } } */

