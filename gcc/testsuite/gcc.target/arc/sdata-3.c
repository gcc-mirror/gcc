/* Check if sdata access is done correctly, specially
   for variables which are having a different alignment
   than the default data type indicates.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

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

/* { dg-final { scan-assembler "ld r2,\\\[gp,@g_a@sda\\\]" } } */
/* { dg-final { scan-assembler "ld.as r2,\\\[gp,@g_b@sda\\\]" } } */
/* { dg-final { scan-assembler "ld\[hw\]\\\.as r2,\\\[gp,@g_c@sda\\\]" } } */
/* { dg-final { scan-assembler "ldb r2,\\\[gp,@g_d@sda\\\]" } } */

/* { dg-final { scan-assembler "st r0,\\\[gp,@g_a@sda\\\]" } } */
/* { dg-final { scan-assembler "st_s r0,\\\[gp,@g_b@sda\\\]" { target { arcem || archs } } } } */
/* { dg-final { scan-assembler "st\\\.as r0,\\\[gp,@g_b@sda\\\]" { target { arc700 || arc6xx } } } } */
/* { dg-final { scan-assembler "st\[hw\]\\\.as r0,\\\[gp,@g_c@sda\\\]" } } */
/* { dg-final { scan-assembler "stb r0,\\\[gp,@g_d@sda\\\]" } } */
