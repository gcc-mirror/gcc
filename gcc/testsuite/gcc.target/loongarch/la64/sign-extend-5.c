/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler-not "slli.w" } } */

typedef struct xpvav XPVAV;
struct xpvav
{
  long int xav_fill;
  long int xav_max;
};
typedef struct av AV;
struct av
{
  XPVAV *sv_any;
  unsigned int sv_refcnt;
  unsigned int sv_flags;
};
void Perl_av_extend (AV *ar, int key);
void
Perl_av_unshift (AV *av, int num)
{
  int i;
  int slide;

  if (num)
    {
      i = ((XPVAV *)(av)->sv_any)->xav_fill;

      slide = i > 0 ? i : 0;
      num += slide;
      Perl_av_extend (av, i + num);

      ((XPVAV *)(av)->sv_any)->xav_max -= slide;
    }
}
