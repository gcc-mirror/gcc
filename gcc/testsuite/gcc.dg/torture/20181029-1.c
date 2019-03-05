/* { dg-do compile } */

typedef __SIZE_TYPE__ size_t;
typedef unsigned long UV;
typedef size_t STRLEN;
typedef struct sv SV;
typedef struct magic MAGIC;
typedef struct xpv XPV;
typedef unsigned char U8;
typedef int I32;
typedef unsigned int U32;
struct sv {
    void* sv_any;
    U32 sv_flags;
};
struct xpv {
    char * xpv_pv;
    STRLEN xpv_cur;
};
struct magic {
    char* mg_ptr;
};
extern const unsigned char PL_utf8skip[];
char *Perl_sv_2pv_flags (STRLEN *);
void Perl_utf8n_to_uvuni (U8 *);
void Perl_sv_magic (SV *);
void Perl_sv_pos_b2u( register SV* sv, I32* offsetp, MAGIC *mg)
{
  U8* s;
  STRLEN len;
  s = (U8*)(((sv)->sv_flags & (0x00040000)) == 0x00040000
	    ? ((len = ((XPV*) (sv)->sv_any)->xpv_cur), ((XPV*) (sv)->sv_any)->xpv_pv)
	    : Perl_sv_2pv_flags(&len));
  if ((I32)len < *offsetp)
    ;
  else
    {
      STRLEN *cache = ((void *)0);
      if (((sv)->sv_flags & (0x00002000|0x00004000|0x00008000))
	  && !((sv)->sv_flags & 0x00800000))
	{
	  if (mg && mg->mg_ptr)
	    {
	      cache = (STRLEN *) mg->mg_ptr;
	      STRLEN forw = *offsetp;
	      STRLEN backw = cache[1] - *offsetp;
	      if (!(forw < 2 * backw))
		{
		  U8 *p = s + cache[1];
		  STRLEN ubackw = 0;
		  cache[1] -= backw;
		  while (backw--)
		    {
		      p--;
		      while ((((U8)*p) >= 0x80 && (((U8)*p) <= 0xbf)))
			{
			  p--;
			  backw--;
			}
		      ubackw++;
		    }
		  cache[0] -= ubackw;
		  *offsetp = cache[0];
		  cache[2] = 0;
		  cache[3] = 0;
		  return;
		}
	    }
	  if (!(((UV)(*s)) < 0x80))
	    Perl_utf8n_to_uvuni(s);
	}
      if (!((sv)->sv_flags & 0x00800000))
	{
	  if (!mg)
	    Perl_sv_magic(sv);
	  cache[0] = len;
	  cache[1] = *offsetp;
	}
      *offsetp = len;
    }
}
