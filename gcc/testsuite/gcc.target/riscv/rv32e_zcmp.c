/* { dg-do compile } */
/* { dg-options " -Os -march=rv32e_zca_zcmp -mabi=ilp32e -mcmodel=medlow" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

char
my_getchar ();
float
getf ();
int __attribute__ ((noinline))
incoming_stack_args (int arg0, int arg1, int arg2, int arg3, int arg4, int arg5,
		     int arg6, int arg7, int arg8);
int
getint ();
void
PrintInts (int n, ...);						 // varargs
void __attribute__ ((noinline)) PrintIntsNoVaStart (int n, ...); // varargs
void
PrintInts2 (int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int n,
	    ...);
extern void
f1 (void);
extern void
f2 (void);

/*
**test1:
**	...
**	cm.push	{ra, s0-s1}, -64
**	...
**	cm.popret	{ra, s0-s1}, 64
**	...
*/
int
test1 ()
{
  char volatile array[3120];
  float volatile farray[3120];

  float sum = 0;
  for (int i = 0; i < 3120; i++)
    {
      array[i] = my_getchar ();
      farray[i] = my_getchar () * 1.2;
      sum += array[i] + farray[i];
    }
  return sum;
}

/*
**test2_step1_0_size:
**	...
**	cm.push	{ra, s0}, -64
**	...
**	cm.popret	{ra, s0}, 64
**	...
*/
int
test2_step1_0_size ()
{
  int volatile iarray[3120 + 1824 / 4 - 8];

  for (int i = 0; i < 3120 + 1824 / 4 - 8; i++)
    {
      iarray[i] = my_getchar () * 2;
    }
  return iarray[0] + iarray[1];
}

/*
**test3:
**	...
**	cm.push	{ra, s0-s1}, -64
**	...
**	cm.popret	{ra, s0-s1}, 64
**	...
*/
float
test3 ()
{
  char volatile array[3120];
  float volatile farray[3120];

  float sum = 0, f1 = 0, f2 = 0, f3 = 0, f4 = 0, f5 = 0, f6 = 0, f7 = 0;

  for (int i = 0; i < 3120; i++)
    {
      f1 = getf ();
      f2 = getf ();
      f3 = getf ();
      f4 = getf ();
      array[i] = my_getchar ();
      farray[i] = my_getchar () * 1.2;
      sum += array[i] + farray[i] + f1 + f2 + f3 + f4;
    }
  return sum;
}

/*
**outgoing_stack_args:
**	...
**	cm.push	{ra, s0}, -32
**	...
**	cm.popret	{ra, s0}, 32
**	...
*/
int
outgoing_stack_args ()
{
  int local = getint ();
  return local + incoming_stack_args (0, 1, 2, 3, 4, 5, 6, 7, 8);
}

/*
**callPrintInts:
**	...
**	cm.push	{ra}, -32
**	...
**	cm.popret	{ra}, 32
**	...
*/
float
callPrintInts ()
{
  volatile float f = getf (); // f in local
  PrintInts (9, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  return f;
}

/*
**callPrint:
**	...
**	cm.push	{ra}, -32
**	...
**	cm.popret	{ra}, 32
**	...
*/
float
callPrint ()
{
  volatile float f = getf (); // f in local
  PrintIntsNoVaStart (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  return f;
}

/*
**callPrint_S:
**	...
**	cm.push	{ra, s0}, -32
**	...
**	cm.popret	{ra, s0}, 32
**	...
*/
float
callPrint_S ()
{
  float f = getf ();
  PrintIntsNoVaStart (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  return f;
}

/*
**callPrint_2:
**	...
**	cm.push	{ra, s0}, -32
**	...
**	cm.popret	{ra, s0}, 32
**	...
*/
float
callPrint_2 ()
{
  float f = getf ();
  PrintInts2 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  return f;
}

/*
**test_step1_0bytes_save_restore:
**	...
**	cm.push	{ra}, -16
**	...
**	cm.popret	{ra}, 16
**	...
*/
int
test_step1_0bytes_save_restore ()
{
  int a = 9;
  int b = my_getchar ();
  return a + b;
}

/*
**test_s0:
**	...
**	cm.push	{ra, s0}, -16
**	...
**	cm.popret	{ra, s0}, 16
**	...
*/
int
test_s0 ()
{
  int a = my_getchar ();
  int b = my_getchar ();
  return a + b;
}

/*
**test_s1:
**	...
**	cm.push	{ra, s0-s1}, -16
**	...
**	cm.popret	{ra, s0-s1}, 16
**	...
*/
int
test_s1 ()
{
  int s0 = my_getchar ();
  int s1 = my_getchar ();
  int b = my_getchar ();
  return s1 + s0 + b;
}

/*
**test_f0:
**	...
**	cm.push	{ra, s0-s1}, -16
**	...
**	cm.popret	{ra, s0-s1}, 16
**	...
*/
int
test_f0 ()
{
  int s0 = my_getchar ();
  float f0 = getf ();
  int b = my_getchar ();
  return f0 + s0 + b;
}

/*
**foo:
**	cm.push	{ra}, -16
**	call	f1(?:@plt)?
**	cm.pop	{ra}, 16
**	tail	f2(?:@plt)?
*/
void
foo (void)
{
  f1 ();
  f2 ();
}

/*
**test_popretz:
**	cm.push	{ra}, -16
**	call	f1(?:@plt)?
**	li	a0,0
**	cm.popret	{ra}, 16
*/
long
test_popretz ()
{
  f1 ();
  return 0;
}
