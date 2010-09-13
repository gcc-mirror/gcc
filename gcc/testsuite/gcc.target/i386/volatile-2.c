/* { dg-do compile } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2" } */

/* Check volatiles are written, read or not re-read consistently */


/* simple assignments */

extern int volatile obj_0;
void test_0 (int data)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_0(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_0(\\(%rip\\))?," } } */
  obj_0 = data;
}

extern int volatile obj_1;
int test_1 (int data)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_1(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_1(\\(%rip\\))?," } } */
  return obj_1 = data;
}

extern int volatile obj_2;
int test_2 (void)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_2(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_2(\\(%rip\\))?," } } */
  return obj_2 = 0;
}


/* Assignments in compound exprs */

extern int volatile obj_3;
int test_3 (int data)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_3(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_3(\\(%rip\\))?," } } */
  return (obj_3 = data, 0);
}

extern int volatile obj_4;
int test_4 (void)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_4(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_4(\\(%rip\\))?," } } */
  return (obj_4 = 0, 0);
}
extern int volatile obj_5;
int test_5 (void)
{
  /* should reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_5(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler "movl\[ \t\]_?obj_5(\\(%rip\\))?," } } */
  return (obj_5 = 0, obj_5);
}

/* Assignments in conditional exprs */

extern int volatile obj_6;
void test_6 (int data, int cond)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_6(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_6(\\(%rip\\))?," } } */
  cond ? obj_6 = data : 0;
}

extern int volatile obj_7;
int test_7 (int data, int cond)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_7(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_7(\\(%rip\\))?," } } */
  return cond ? obj_7 = data : 0;
}

extern int volatile obj_8;
int test_8 (int cond)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, _?obj_8(\\(%rip\\))?" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]_?obj_8(\\(%rip\\))?," } } */
  return cond ? obj_8 = 0 : 0;
}
