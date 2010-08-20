/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check volatiles are written, read or not re-read consistently */


/* simple assignments */

extern int volatile obj_0;
void test_0 (int data)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_0" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_0," } } */
  obj_0 = data;
}

extern int volatile obj_1;
int test_1 (int data)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_1" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_1," } } */
  return obj_1 = data;
}

extern int volatile obj_2;
int test_2 (void)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_2" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_2," } } */
  return obj_2 = 0;
}


/* Assignments in compound exprs */

extern int volatile obj_3;
int test_3 (int data)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_3" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_3," } } */
  return (obj_3 = data, 0);
}

extern int volatile obj_4;
int test_4 (void)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_4" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_4," } } */
  return (obj_4 = 0, 0);
}
extern int volatile obj_5;
int test_5 (void)
{
  /* should reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_5" } } */
  /* { dg-final { scan-assembler "movl\[ \t\]obj_5," } } */
  return (obj_5 = 0, obj_5);
}

/* Assignments in conditional exprs */

extern int volatile obj_6;
void test_6 (int data, int cond)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_6" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_6," } } */
  cond ? obj_6 = data : 0;
}

extern int volatile obj_7;
int test_7 (int data, int cond)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_7" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_7," } } */
  return cond ? obj_7 = data : 0;
}

extern int volatile obj_8;
int test_8 (int cond)
{
  /* should not reread obj */
  /* { dg-final { scan-assembler "movl\[ \t\]\[^,\]+, obj_8" } } */
  /* { dg-final { scan-assembler-not "movl\[ \t\]obj_8," } } */
  return cond ? obj_8 = 0 : 0;
}
