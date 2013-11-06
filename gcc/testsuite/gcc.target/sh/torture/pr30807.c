/* { dg-do compile }  */
/* { dg-additional-options "-fpic -std=c99" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */

typedef unsigned int size_t;
typedef struct
{
  unsigned long __val[(1024 / (8 * sizeof (unsigned long)))];
}  __sigset_t;
struct __jmp_buf_tag
{
  __sigset_t __saved_mask;
};
typedef struct __jmp_buf_tag sigjmp_buf[1];
struct stat
{
  long long st_dev;
  unsigned short int __pad1;
  int tm_isdst;
  long int tm_gmtoff;
  char *tm_zone;
};

typedef size_t STRLEN;
typedef struct op OP;
typedef struct cop COP;
typedef struct interpreter PerlInterpreter;
typedef struct sv SV;
typedef struct av AV;
typedef struct cv CV;
typedef struct gp GP;
typedef struct gv GV;
typedef struct xpv XPV;
typedef struct xpvio XPVIO;
typedef union any ANY;
typedef unsigned char U8;
typedef long I32;
typedef unsigned long U32;
typedef U32 line_t;
typedef struct _PerlIO PerlIOl;
typedef PerlIOl *PerlIO;
struct sv
{
  void *sv_any;
  U32 sv_flags;
  union
  {
    char *svu_pv;
  } sv_u;
};
struct gv
{
  U32 sv_flags;
  union
  {
    GP *svu_gp;
  } sv_u;
};
struct io
{
  XPVIO *sv_any;
};
struct xpv
{
  STRLEN xpv_cur;
};
struct xpvio
{
  PerlIO *xio_ofp;
};
struct gp
{
  SV *gp_sv;
  struct io *gp_io;
};
struct jmpenv
{
  struct jmpenv *je_prev;
  sigjmp_buf je_buf;
  int je_ret;
};
typedef struct jmpenv JMPENV;
struct cop
{
  line_t cop_line;
  struct refcounted_he *cop_hints_hash;
};
struct interpreter
{
  SV **Istack_sp;
  OP *Iop;
  SV **Icurpad;
  SV **Istack_base;
  SV **Istack_max;
  I32 *Iscopestack;
  I32 Iscopestack_ix;
  I32 Iscopestack_max;
  ANY *Isavestack;
  I32 Isavestack_ix;
  I32 Isavestack_max;
  SV **Itmps_stack;
  I32 Itmps_ix;
  I32 Itmps_floor;
  I32 Itmps_max;
  I32 Imodcount;
  I32 *Imarkstack;
  I32 *Imarkstack_ptr;
  I32 *Imarkstack_max;
  SV *ISv;
  XPV *IXpv;
  STRLEN Ina;
  struct stat Istatbuf;
  struct stat Istatcache;
  OP *Irestartop;
  COP *volatile Icurcop;
  JMPENV *Itop_env;
  U8 Iexit_flags;
  I32 Istatusvalue;
  I32 Istatusvalue_posix;
  GV *Istderrgv;
  GV *Ierrgv;
  AV *Ibeginav;
  AV *Iunitcheckav;
  COP Icompiling;
  char Isavebegin;
  volatile U32 Idebug;
  AV *Ibeginav_save;
  AV *Icheckav_save;
  AV *Iunitcheckav_save;
};

void S_my_exit_jump (PerlInterpreter *my_perl __attribute__((unused)))
  __attribute__((noreturn));

int Perl_av_len (PerlInterpreter*, AV*);
void Perl_av_create_and_push (PerlInterpreter*, AV**, SV*);
int __sigsetjmp (sigjmp_buf env, int savemask);
void Perl_sv_2pv_flags (PerlInterpreter*, SV*, STRLEN*, int);
void Perl_deb (PerlInterpreter*,
	       const char*, const char*, int, const char*, int);
void Perl_croak (PerlInterpreter*, const char*, void*);
void foo (void);

void
Perl_call_list (PerlInterpreter *my_perl __attribute__((unused)),
		I32 oldscope, AV *paramList)
{
  SV *atsv;
  CV *cv;
  STRLEN len;
  int ret;
  JMPENV cur_env;
  GV *shplep;
  volatile line_t oldline;

  oldline = (my_perl->Icurcop) ? my_perl->Icurcop->cop_line : 0;

  while (Perl_av_len (my_perl, paramList) >= 0)
    {
      if (my_perl->Isavebegin)
	{
	  if (paramList == my_perl->Ibeginav)
	    {
	      Perl_av_create_and_push (my_perl, &my_perl->Ibeginav_save,
				       (SV*) cv);
	      Perl_av_create_and_push(my_perl, &my_perl->Icheckav_save,
				      (SV*) cv);
	    }
	  else if (paramList == my_perl->Iunitcheckav)
	    Perl_av_create_and_push(my_perl, &my_perl->Iunitcheckav_save,
				    (SV*) cv);
	}

      cur_env.je_ret = __sigsetjmp (cur_env.je_buf, 0);

      switch (ret)
	{
	case 0:
	  shplep = (GV *) my_perl->Ierrgv;
	  *my_perl->Imarkstack_ptr = my_perl->Istack_sp - my_perl->Istack_base;
	  atsv = shplep->sv_u.svu_gp->gp_sv;
	  if (atsv->sv_flags & 0x00000400 == 0x00000400)
	    len = ((XPV*) ((SV *) atsv)->sv_any)->xpv_cur;
	  else
	    Perl_sv_2pv_flags (my_perl, atsv, &len, 2|32);

	  if (len)
	    {
	      my_perl->Icurcop = &my_perl->Icompiling;
	      while (my_perl->Iscopestack_ix > oldscope)
		{
		  if (my_perl->Idebug & 0x00000004)
		    Perl_deb (my_perl, "scope", "LEAVE",
			      my_perl->Iscopestack_ix, "perl.c", 5166);
		  (my_perl->Itop_env) = cur_env.je_prev;
		}

	      Perl_croak (my_perl, "%""-p""", (void*) atsv);
	    }

	case 1:
	  my_perl->Istatusvalue = 1;
	  my_perl->Istatusvalue_posix = 1;
	case 2:
	  while (my_perl->Iscopestack_ix > oldscope)
	    if (my_perl->Idebug & 0x00000004)
	      foo ();
	  my_perl->Icurcop = &my_perl->Icompiling;
	  my_perl->Icurcop->cop_line = oldline;
	  if (my_perl->Idebug & 0x00000004)
	    foo ();
	  S_my_exit_jump (my_perl);
	case 3:
	  if (my_perl->Irestartop)
	    foo ();
	}
    }
}
