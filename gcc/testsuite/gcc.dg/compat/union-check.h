/* Function definitions that are used by multiple tests.  */

#define CHECK_CHAR(TYPE)		\
  void check##TYPE (TYPE p, int i)	\
   { if (p.c != (char)i) DEBUG_CHECK }

CHECK_CHAR(Ucs)
CHECK_CHAR(Uci)
CHECK_CHAR(Ucl)
CHECK_CHAR(Ucll)


#define CHECK_SHORT(TYPE)		\
  void check##TYPE (TYPE p, int i)	\
   { if (p.s != (short)i) DEBUG_CHECK }

CHECK_SHORT(Usi)
CHECK_SHORT(Usl)
CHECK_SHORT(Usll)


#define CHECK_INT(TYPE)			\
  void check##TYPE (TYPE p, int i)	\
   { if (p.i != i) DEBUG_CHECK }

CHECK_INT(Uil)
CHECK_INT(Uill)


#define CHECK_LONG(TYPE)		\
  void check##TYPE (TYPE p, int i)	\
   { if (p.l != (long)i) DEBUG_CHECK }

CHECK_LONG(Ulll)
