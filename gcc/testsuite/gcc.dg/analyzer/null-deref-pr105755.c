/* { dg-additional-options "-Wno-analyzer-too-complex -O2" } */

typedef long int ptrdiff_t;
typedef long int EMACS_INT;
typedef long int intmax_t;

enum Lisp_Type
  {
    Lisp_Symbol = 0,
    Lisp_Vectorlike = 5,
  };
typedef struct Lisp_X *Lisp_Object;

static inline EMACS_INT
XLI (Lisp_Object o)
{
  return (EMACS_INT) o;
}

static inline void *
XLP (Lisp_Object o)
{
  return (void *) o;
}

struct Lisp_Symbol
{
  Lisp_Object name;
  Lisp_Object value;
};
extern struct Lisp_Symbol lispsym[1455];

union vectorlike_header
  {
    ptrdiff_t size;
  };
static inline _Bool
TAGGEDP (Lisp_Object a, enum Lisp_Type tag)
{
  return (! (((unsigned) XLI (a) - (unsigned) (tag)) & 7));
}

struct Lisp_Symbol_With_Pos
{
  union vectorlike_header header;
  Lisp_Object sym;
  Lisp_Object pos;
};

static inline _Bool
PSEUDOVECTORP (Lisp_Object a, int code)
{
  return (TAGGEDP (a, Lisp_Vectorlike)
	  && ((((union vectorlike_header *)
		((char *) XLP ((a)) - Lisp_Vectorlike))->size
	       & 0x400000003f000000)
	      == (0x4000000000000000 | (code << 24))));
}

static inline _Bool
BARE_SYMBOL_P (Lisp_Object x)
{
  return TAGGEDP (x, Lisp_Symbol);
}

static inline _Bool
SYMBOL_WITH_POS_P (Lisp_Object x)
{
  return PSEUDOVECTORP (x, 6);
}

static inline struct Lisp_Symbol_With_Pos *
XSYMBOL_WITH_POS (Lisp_Object a)
{
  return (struct Lisp_Symbol_With_Pos *) ((char *) XLP (a) - Lisp_Vectorlike);
}

static inline Lisp_Object
make_lisp_symbol (struct Lisp_Symbol *sym)
{
  return (Lisp_Object) ((char *) sym - (char *) lispsym);
}

static inline Lisp_Object
builtin_lisp_symbol (int index)
{
  return make_lisp_symbol (&lispsym[index]);
}

static inline _Bool
BASE_EQ (Lisp_Object x, Lisp_Object y)
{
  return XLI (x) == XLI (y);
}

extern _Bool symbols_with_pos_enabled;

static inline _Bool
EQ (Lisp_Object x, Lisp_Object y)
{
  return (XLI (x) == XLI (y)
	  || (symbols_with_pos_enabled
	      && (SYMBOL_WITH_POS_P (x)
		  ? (BARE_SYMBOL_P (y)
		     ? XLI (XSYMBOL_WITH_POS(x)->sym) == XLI (y)
		     : (SYMBOL_WITH_POS_P(y)
			&& (XLI (XSYMBOL_WITH_POS(x)->sym)
			    == XLI (XSYMBOL_WITH_POS(y)->sym))))
		  : (SYMBOL_WITH_POS_P (y) && BARE_SYMBOL_P (x)
		     && (XLI (x) == XLI ((XSYMBOL_WITH_POS (y))->sym))))));
}

static inline _Bool
NILP (Lisp_Object x)
{
  return BASE_EQ (x, builtin_lisp_symbol (0));
}

static inline _Bool
ASCII_CHAR_P (intmax_t c)
{
  return 0 <= c && c < 0x80;
}

struct Lisp_Char_Table
  {
    union vectorlike_header header;
    Lisp_Object defalt;
    Lisp_Object parent;
    Lisp_Object ascii;
};

extern Lisp_Object char_table_ref (Lisp_Object, int);

static inline _Bool
CHAR_TABLE_P (Lisp_Object a)
{
  return PSEUDOVECTORP (a, 28);
}

static inline struct Lisp_Char_Table *
XCHAR_TABLE (Lisp_Object a)
{
  return (struct Lisp_Char_Table *) ((char *) XLP (a) - Lisp_Vectorlike);
}

struct Lisp_Sub_Char_Table
{
  union vectorlike_header header;
  Lisp_Object contents[];
};

static inline _Bool
SUB_CHAR_TABLE_P (Lisp_Object a)
{
  return PSEUDOVECTORP (a, 29);
}

static inline struct Lisp_Sub_Char_Table *
XSUB_CHAR_TABLE (Lisp_Object a)
{
  return (struct Lisp_Sub_Char_Table *) ((char *) XLP (a) - Lisp_Vectorlike);
}

static inline Lisp_Object
CHAR_TABLE_REF_ASCII (Lisp_Object ct, ptrdiff_t idx)
{
  for (struct Lisp_Char_Table *tbl = XCHAR_TABLE (ct); ;
       tbl = XCHAR_TABLE (tbl->parent))
    {
      Lisp_Object val = (! SUB_CHAR_TABLE_P (tbl->ascii) ? tbl->ascii
			 : XSUB_CHAR_TABLE (tbl->ascii)->contents[idx]);
      if (NILP (val))
	val = tbl->defalt;
      if (!NILP (val) || NILP (tbl->parent))
	return val;
    }
}

static inline Lisp_Object
CHAR_TABLE_REF (Lisp_Object ct, int idx)
{
  return (ASCII_CHAR_P (idx)
	  ? CHAR_TABLE_REF_ASCII (ct, idx)
	  : char_table_ref (ct, idx));
}

_Bool
word_boundary_p (Lisp_Object char_script_table, int c1, int c2)
{
  return EQ (CHAR_TABLE_REF (char_script_table, c1),
	     CHAR_TABLE_REF (char_script_table, c2));
}
