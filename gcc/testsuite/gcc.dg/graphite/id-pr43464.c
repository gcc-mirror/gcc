typedef struct sv SV;
typedef struct regnode
{
  char flags;
} regnode;
typedef struct regexp
{
} regexp;
typedef struct cop
{
  SV *cop_warnings;
} COP;
extern const unsigned char PL_utf8skip[];
extern char PL_dowarn;
extern COP *volatile PL_curcop;
int S_reginclass (regnode *);
int S_regtry (regexp *, char *);
unsigned long Perl_utf8n_to_uvuni (char *, int, long unsigned int *, int);
char *
S_find_byclass (regexp * prog, regnode * c, char *s, char *strend,
		char *startpos, int norun)
{
  register long unsigned int uskip;
  char *e;
  switch (((c)->flags))
    {
    case 17:
	{
	  while (s + (uskip = PL_utf8skip[*s]) <= strend)
	    {
	      if (c->flags || S_reginclass (c))
		  if (norun || S_regtry (prog, s))
		    goto got_it;
	      s += uskip;
	    }
	  unsigned long c, f;
	  long unsigned int len;
	    {
	      while (s <= e)
		{
		  c = Perl_utf8n_to_uvuni (s, 13, &len,
					   (((PL_curcop->cop_warnings !=
					      ((SV *) ((void *) 0)))
					     && PL_dowarn)) ? 0 : 0x00FF);
		  if (c == 0 && (norun || S_regtry (prog, s)))
		      if (f != c && (norun || S_regtry (prog, s)))
			goto got_it;
		}
	    }
	}
    }
got_it:
  return s;
}
void
Perl_re_intuit_start (regexp * prog)
{
  S_find_byclass (prog, 0, 0, 0, 0, 1);
}
void
Perl_regexec_flags (register regexp * prog,
		    register char *strend)
{
  S_find_byclass (prog, 0, 0, strend, 0, 0);
}
int
S_regtry (regexp * prog, char *startpos)
{
}
