#define TEST(NUM,TMODE,VAL)				\
extern v##NUM##TMODE g_v##NUM##TMODE;			\
extern TMODE g_##TMODE;					\
							\
extern void pass_v##NUM##TMODE (v##NUM##TMODE);		\
extern v##NUM##TMODE return_v##NUM##TMODE (void);	\
							\
void							\
checkp_##NUM##TMODE (TMODE *p)				\
{							\
  int i;						\
  for (i = 0; i < NUM; i++)				\
    {							\
      if (p[i] != g_##TMODE + i)			\
	DEBUG_CHECK;					\
    }							\
}							\
							\
void							\
checkg_##NUM##TMODE (void)				\
{							\
  u##NUM##TMODE u;					\
  TMODE *p = u.a;					\
							\
  u.v = g_v##NUM##TMODE;				\
  checkp_##NUM##TMODE (p);				\
}							\
							\
void							\
init_##NUM##TMODE (void)				\
{							\
  int i;						\
  u##NUM##TMODE u;					\
  g_##TMODE = VAL;					\
  for (i = 0; i < NUM; i++)				\
    u.a[i] = VAL + i;					\
  g_v##NUM##TMODE = u.v;				\
}							\
							\
void							\
test_v##NUM##TMODE (void)				\
{							\
  v##NUM##TMODE v;					\
  u##NUM##TMODE u;					\
  TMODE *p = u.a;					\
							\
  DEBUG_FPUTS ("v" #NUM #TMODE);			\
  DEBUG_NL;						\
  DEBUG_FPUTS ("  global variable:");			\
  init_##NUM##TMODE ();					\
  checkg_##NUM##TMODE ();				\
  DEBUG_NL;						\
  DEBUG_FPUTS ("  pass global variable:");		\
  pass_v##NUM##TMODE (g_v##NUM##TMODE);			\
  DEBUG_NL;						\
  DEBUG_FPUTS ("  pass local variable:");		\
  v = g_v##NUM##TMODE;					\
  pass_v##NUM##TMODE (v);				\
  DEBUG_NL;						\
  DEBUG_FPUTS ("  function return:");			\
  u.v = return_v##NUM##TMODE ();			\
  checkp_##NUM##TMODE (p);				\
  DEBUG_NL;						\
}
