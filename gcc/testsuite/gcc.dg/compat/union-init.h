/* Function definitions that are used by multiple tests.  */

#define INIT_CHAR(TYPE)			\
  void init##TYPE (TYPE *p, int i)	\
   { p->c = (char)i; }

INIT_CHAR(Ucs)
INIT_CHAR(Uci)
INIT_CHAR(Ucl)
INIT_CHAR(Ucll)


#define INIT_SHORT(TYPE)		\
  void init##TYPE (TYPE *p, int i)	\
   { p->s = (short)i; }

INIT_SHORT(Usi)
INIT_SHORT(Usl)
INIT_SHORT(Usll)


#define INIT_INT(TYPE)			\
  void init##TYPE (TYPE *p, int i)	\
   { p->i = i; }

INIT_INT(Uil)
INIT_INT(Uill)


#define INIT_LONG(TYPE)			\
  void init##TYPE (TYPE *p, int i)	\
   { p->l = (long)i; }

INIT_LONG(Ulll)
