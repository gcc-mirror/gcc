#if DEBUG
#include <stdio.h>
#define DBG(x) fputs (x, stdout); fflush (stdout);
#else
#define DBG(x)
#endif

#define TEST_FUNCS(NAME, TYPE, PADT, VAL, VAL2) \
extern void NAME##_f0 (TYPE a, PADT b);					\
extern void NAME##_f1 (PADT z0, TYPE a, PADT b);			\
extern void NAME##_f2 (PADT z0, PADT z1, TYPE a, PADT b);		\
extern void NAME##_f3 (PADT z0, PADT z1, PADT z2, TYPE a, PADT b);	\
extern void NAME##_f4 (PADT z0, PADT z1, PADT z2, PADT z3, TYPE a,	\
		       PADT b);						\
extern void NAME##_f5 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4,	\
		       TYPE a, PADT b);					\
extern void NAME##_f6 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4,	\
		       PADT z5, TYPE a, PADT b);			\
extern void NAME##_f7 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4,	\
		       PADT z5, PADT z6, TYPE a, PADT b);		\
extern void NAME##_f8 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4,	\
		       PADT z5, PADT z6, PADT z7, TYPE a, PADT b);	\
extern void NAME##_f9 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4,	\
		       PADT z5, PADT z6, PADT z7, PADT z8, TYPE a,	\
		       PADT b);						\
extern void NAME##_fv (int n, ...);					\
									\
void NAME##_doit (void)							\
{									\
  NAME##_f0 (VAL, VAL2);						\
  DBG (".");								\
  NAME##_f1 ((PADT) 0, VAL, VAL2);					\
  DBG (".");								\
  NAME##_f2 ((PADT) 0, (PADT) 0, VAL, VAL2);				\
  DBG (".");								\
  NAME##_f3 ((PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);			\
  DBG (".");								\
  NAME##_f4 ((PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);	\
  DBG (".");								\
  NAME##_f5 ((PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,		\
	     VAL, VAL2);						\
  DBG (".");								\
  NAME##_f6 ((PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,		\
	     (PADT) 0, VAL, VAL2);					\
  DBG (".");								\
  NAME##_f7 ((PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,		\
	     (PADT) 0, (PADT) 0, VAL, VAL2);				\
  DBG (".");								\
  NAME##_f8 ((PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,		\
	     (PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);			\
  DBG (".");								\
  NAME##_f9 ((PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,		\
	     (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);	\
  DBG (".");								\
  NAME##_fv (0, VAL, VAL2);						\
  DBG (".");								\
  NAME##_fv (1, (PADT) 0, VAL, VAL2);					\
  DBG (".");								\
  NAME##_fv (2, (PADT) 0, (PADT) 0, VAL, VAL2);				\
  DBG (".");								\
  NAME##_fv (3, (PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);		\
  DBG (".");								\
  NAME##_fv (4, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);	\
  DBG (".");								\
  NAME##_fv (5, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,	\
	     VAL, VAL2);						\
  DBG (".");								\
  NAME##_fv (6, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,	\
	     (PADT) 0, VAL, VAL2);					\
  DBG (".");								\
  NAME##_fv (7, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,	\
	     (PADT) 0, (PADT) 0, VAL, VAL2);				\
  DBG (".");								\
  NAME##_fv (8, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0, (PADT) 0,	\
	     (PADT) 0, (PADT) 0, (PADT) 0, VAL, VAL2);			\
  DBG ("\n");								\
}
