/* Check if sdata access is done correctly, specially
   for variables which are having a different alignment
   than the default data type indicates.  */
/* { dg-do compile } */
/* { dg-options "-O2 -msdata" } */

int g_a __attribute__ ((aligned (1)));
int g_b;
short g_c;
char g_d;

#define TEST(name, optype)			\
  optype testLD_ ## name (optype x)		\
  {						\
    return g_ ## name + x;			\
  }						\
  void testST_ ## name (optype x)		\
  {						\
    g_ ## name = x;				\
  }

TEST (a, int)
TEST (b, int)
TEST (c, short)
TEST (d, char)

/* { dg-final { scan-assembler "ld\\s+r2,\\\[gp,@g_a@sda\\\]" } } */
/* { dg-final { scan-assembler "ld.as\\s+r2,\\\[gp,@g_b@sda\\\]" } } */
/* { dg-final { scan-assembler "ld\[hw\]\\\.as\\s+r2,\\\[gp,@g_c@sda\\\]" } } */
/* { dg-final { scan-assembler "ldb\\s+r2,\\\[gp,@g_d@sda\\\]" } } */

/* { dg-final { scan-assembler "st\\s+r0,\\\[gp,@g_a@sda\\\]" } } */
/* { dg-final { scan-assembler "st_s\\s+r0,\\\[gp,@g_b@sda\\\]" { target { codedensity } } } } */
/* { dg-final { scan-assembler "st\\\.as\\s+r0,\\\[gp,@g_b@sda\\\]" { target { ! { codedensity } } } } } */
/* { dg-final { scan-assembler "st\[hw\]\\\.as\\s+r0,\\\[gp,@g_c@sda\\\]" } } */
/* { dg-final { scan-assembler "stb\\s+r0,\\\[gp,@g_d@sda\\\]" } } */
