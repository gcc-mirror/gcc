/* { dg-do compile } */
/* { dg-options "-mrecip=all  -mfrecipe -mabi=lp64d -march=loongarch64 -mfpu=64 -msimd=lasx -Ofast" } */

typedef long unsigned int STRLEN;
typedef struct sv SV;
struct sv
{
  void *sv_any;
  unsigned int sv_refcnt;
  unsigned int sv_flags;
};
typedef struct xpv XPV;
struct xpv
{
  char *xpv_pv;
  STRLEN xpv_cur;
  STRLEN xpv_len;
};
typedef unsigned long UV;
extern char *PL_bufend;
extern char *d;
SV *Perl_newSV (STRLEN len);

char *
S_scan_const (char *start)
{
  register char *send = PL_bufend;
  SV *sv = Perl_newSV (send - start);
  register char *s = start;
  UV uv;

  while (s < send)
    {
      if (!(((UV)(uv)) < 0x80))
        {
          int hicount = 0;
          unsigned char *c;
          for (c = (unsigned char *)((XPV *)(sv)->sv_any)->xpv_pv;
               c < (unsigned char *)d; c++)
            {
              if (!(((UV)(*c)) < 0x80))
                {
                  hicount++;
                }
            }
          d += hicount;
          *d++ = (char)uv;
        }

      s++;
    }

  return s;
}
