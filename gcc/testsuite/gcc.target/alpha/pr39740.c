/* { dg-do compile } */
/* { dg-options "-O3 -std=c99 -mexplicit-relocs" } */

typedef int R_len_t;
typedef unsigned int SEXPTYPE;
struct sxpinfo_struct
{
  SEXPTYPE type:5;
};

struct vecsxp_struct
{
  R_len_t length;
  R_len_t truelength;
};

struct listsxp_struct
{
  struct SEXPREC *carval;
  struct SEXPREC *cdrval;
  struct SEXPREC *tagval;
};

typedef struct SEXPREC
{
  struct sxpinfo_struct sxpinfo;
  union
  {
    struct listsxp_struct listsxp;
  } u;
} SEXPREC, *SEXP;

typedef struct VECTOR_SEXPREC
{
  struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;

typedef union
{
  VECTOR_SEXPREC s;
  double align;
} SEXPREC_ALIGN;

extern SEXP R_NilValue;
extern SEXP R_MissingArg;

int Rf_envlength (SEXP rho);
SEXP Rf_protect (SEXP);
const char *Rf_translateChar (SEXP);

inline R_len_t
Rf_length (SEXP s)
{
  int i;
  switch (((s)->sxpinfo.type))
    {
    case 0:
      return 0;
    case 24:
      return (((VECSEXP) (s))->vecsxp.length);
    case 6:
    case 17:
      i = 0;
      while (s != ((void *) 0) && s != R_NilValue)
	{
	  i++;
	  s = ((s)->u.listsxp.cdrval);
	}
      return i;
    case 4:
      return Rf_envlength (s);
    default:
      return 1;
    }
}

inline SEXP
Rf_lang3 (SEXP s, SEXP t, SEXP u)
{
  return s;
}

typedef SEXP (*CCODE) (SEXP, SEXP, SEXP, SEXP);

static SEXP PlusSymbol;
static SEXP MinusSymbol;
static SEXP DivideSymbol;

int isZero (SEXP s);
SEXP PP (SEXP s);
SEXP AddParens (SEXP expr);
SEXP Rf_install ();

static int
isUminus (SEXP s)
{
  if (((s)->sxpinfo.type) == 6 && ((s)->u.listsxp.carval) == MinusSymbol)
    {
      switch (Rf_length (s))
	{
	case 2:
	  return 1;
	case 3:
	  if (((((((s)->u.listsxp.cdrval))->u.listsxp.cdrval))->u.listsxp.
	       carval) == R_MissingArg)
	    return 1;
	  else
	    return 0;
	}
    }
  else
    return 0;
}

static SEXP
simplify (SEXP fun, SEXP arg1, SEXP arg2)
{
  SEXP ans;
  if (fun == PlusSymbol)
    {
      if (isZero (arg1))
	ans = arg2;
      else if (isUminus (arg1))
	ans =
	  simplify (MinusSymbol, arg2,
		    ((((arg1)->u.listsxp.cdrval))->u.listsxp.carval));
      else if (isUminus (arg2))
	ans =
	  simplify (MinusSymbol, arg1,
		    ((((arg2)->u.listsxp.cdrval))->u.listsxp.carval));
    }
  else if (fun == DivideSymbol)
    {
      ans = Rf_lang3 (DivideSymbol, arg1, arg2);
    }

  return ans;
}


static SEXP
D (SEXP expr, SEXP var)
{
  return simplify (PlusSymbol,
		   PP (D
		       (((((expr)->u.listsxp.cdrval))->u.listsxp.carval),
			var)),
		   PP (D
		       (((((((expr)->u.listsxp.cdrval))->u.listsxp.cdrval))->
			 u.listsxp.carval), var)));
}

SEXP
do_D (SEXP call, SEXP op, SEXP args, SEXP env)
{
  SEXP expr, var;
  var = Rf_install ();
  expr = ((args)->u.listsxp.carval);
  Rf_protect (expr = D (expr, var));
  expr = AddParens (expr);
  return expr;
}
