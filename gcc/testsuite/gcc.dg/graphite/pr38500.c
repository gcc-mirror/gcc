/* { dg-options "-O3 -fgraphite-identity" } */

typedef unsigned char U8;
extern char test1;

char *Perl_screaminstr (int, int, int);
int Perl_utf8_distance (U8 *, U8 *);

void
Perl_re_intuit_start( int minlen,  char *strend, unsigned int flags, int i)
{
  register int start_shift = 0;
  register int end_shift = 0;
  register char *s;
  char *strbeg;
  char *t;
  if(i > 0)
    goto success_at_start;
  int end = 0;
  int eshift = (test1 ? Perl_utf8_distance((U8*)strend,(U8*)s) : (U8*)strend - (U8*)s) - end;
  if (end_shift < eshift)
    end_shift = eshift;
 restart:
  s = Perl_screaminstr(start_shift + (s - strbeg), end_shift, 0);
  while( t < strend - minlen){  
  }
 success_at_start:
  eshift = (test1 ? Perl_utf8_distance((U8*)strend,(U8*)s) : (U8*)strend - (U8*)s) - end;
  goto restart;
}
