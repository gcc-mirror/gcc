/* { dg-additional-options -O2 } */
/* { dg-additional-options -fdump-tree-optimized } */
/* { dg-require-effective-target int32plus } */

/* Code taken from Perlbench */

typedef long unsigned int size_t;
typedef long int ssize_t;
typedef size_t STRLEN;
typedef struct cop COP;
typedef struct sv SV;
typedef struct p5rx REGEXP;
typedef struct magic MAGIC;
typedef unsigned char U8;
typedef int I32;
typedef unsigned int U32;

typedef int boolean;

struct sv
{
  void *sv_any;
  U32 sv_flags;
  union
  {
    SV *svu_rv;
  } sv_u;
};
struct p5rx
{
  union
  {
    struct regexp *svu_rx;
  } sv_u;
};
typedef struct regexp
{
  U32 extflags;
} regexp;
typedef struct
{
  char *ganch;
} regmatch_info;
struct cop
{
  U32 cop_hints;
};
struct magic
{
  U8 mg_flags;
  ssize_t mg_len;
};
MAGIC *Perl_mg_find_mglob (SV * sv);
extern COP *PL_curcop;
extern STRLEN S_MgBYTEPOS (MAGIC *mg, SV *sv, const char *s, STRLEN len) ;

static struct regexp *
S_ReANY (const REGEXP *const re)
{
  return re->sv_u.svu_rx;
}


I32
Perl_regexec_flags (REGEXP *const rx, char *stringarg, char *strend,
                    char *strbeg, ssize_t minend, SV *sv, void *data,
                    U32 flags)
{
  char *startpos;
  ssize_t minlen;
  MAGIC *mg;

  const boolean
    utf8_target =
    (((((sv)->sv_flags & 0x20000000)
       && !(((PL_curcop)->cop_hints + 0) & 0x00000008))) ? 1 : 0);
  regmatch_info reginfo_buf;
  regmatch_info *const reginfo = &reginfo_buf;

  startpos = stringarg;

  reginfo->ganch =
    (flags & 0x08)
    ? stringarg
    : ((mg = Perl_mg_find_mglob (sv)) && mg->mg_len >= 0)
    ? strbeg + S_MgBYTEPOS (mg, sv, strbeg, strend - strbeg) : strbeg;

  if ((startpos + minlen) > strend || startpos < strbeg)
      return 0;

  ((utf8_target)
   ? (((S_ReANY ((const REGEXP *) (rx)))->extflags) |=
      (1U << (((0 + 12) + 2) + 6)))
   : (((S_ReANY ((const REGEXP *) (rx)))->extflags) &=
      ~(1U << (((0 + 12) + 2) + 6))));

}
/* { dg-final { scan-tree-dump-times " \\\| 1048576" 0 optimized } } */
