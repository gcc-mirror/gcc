#define SETUP(NUM,TMODE)				\
v##NUM##TMODE g_v##NUM##TMODE;				\
TMODE g_##TMODE;					\
							\
extern void test_v##NUM##TMODE (void);			\
extern void checkp_##NUM##TMODE (TMODE *);		\
							\
void							\
pass_v##NUM##TMODE (v##NUM##TMODE v)			\
{							\
  u##NUM##TMODE u;					\
  int j;						\
  TMODE a[NUM];						\
  							\
  u.v = v;						\
  for (j = 0; j < NUM; j++)				\
    a[j] = u.a[j];					\
  checkp_##NUM##TMODE (a);				\
}							\
							\
v##NUM##TMODE						\
return_v##NUM##TMODE (void)				\
{							\
  return g_v##NUM##TMODE;				\
}

#define CHECK(NUM,TMODE)				\
  test_v##NUM##TMODE()
