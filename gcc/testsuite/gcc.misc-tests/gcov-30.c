/* { dg-options "--coverage -fpath-coverage -fprofile-update=atomic" } */
/* { dg-do run } */
/* { dg-require-effective-target profile_update_atomic } */

void
pathcov001a ()
{
  /* Empty functions should not be problematic.  */
}

/* Straight line, which should have only one path.  */
/* BEGIN paths
   summary: 1/1
*/
void
pathcov001b ()
/* END */
{
  int a = 0;
  ++a;
}

/* Same as b, but not executed.  */
/* BEGIN paths
   summary: 0/1
   expect: 34
*/
void
pathcov001c ()
/* END */
{
  int a = 0;
  ++a;
}

/* 002 is a simple baseline test, with no complicated control flow and no
   loops, run with different combinations of inputs that tests the paths in
   isolation.  */
/* BEGIN paths
   summary: 0/2
   expect: 49(true) 50 53
   expect: 49(false) 52 53
*/
void
pathcov002a (int a)
/* END */
{
  int v = 0;
  if (a)
    v++;
  else
    v--;
}

/* BEGIN paths
    summary: 1/2
    expect: 64(false) 67 68
*/
void
pathcov002c (int a)
/* END */
{
  int v = 0;
  if (a)
    v++;
  else
    v--;
}

/* BEGIN paths
   summary: 1/2
   expect: 79(true) 80 83
*/
void
pathcov002b (int a)
/* END */
{
  int v = 0;
  if (a)
    v++;
  else
    v--;
}

/* Identical to 002*, but run for both inputs.  This should achieve full
   coverage.

   BEGIN paths
   summary: 2/2
*/
void
pathcov002d (int a)
/* END */
{
  int v = 0;
  if (a)
    v++;
  else
    v--;
}

/* Test individual control flow structures in isolation.  */

/* BEGIN paths
   summary: 0/2
   expect: 113(true) 114 115
   expect: 113(false) 115
*/
void
pathcov003a (int a)
/* END */
{
  if (a)
    a++;
}

/* BEGIN paths
   summary: 0/2
   expect: 126(true) 127 130
   expect: 126(false) 129 130
*/
void
pathcov003b (int a)
/* END */
{
  if (a)
    a++;
  else
    a--;
}

/* BEGIN paths
   summary: 0/3
   expect: 142(true) 143 148
   expect: 142(false) 144(true) 145 148
   expect: 142(false) 144(false) 147 148
*/
void
pathcov003c (int a)
/* END */
{
  if (a > 10)
    a++;
  else if (a > 20)
    a--;
  else
    a += 2;
}

/* BEGIN paths
   summary: 0/5
   expect: 163 163(true) 164
   expect: 163 163(false) 165
   expect: 164 163(true) 164
   expect: 164 163(false) 165
   expect: 163(true) 164 163
*/
void
pathcov003d (int a)
/* END */
{
  int x = 0;
  for (int i = 0; i < a; ++i)
    ++x;
}

/* BEGIN paths
   summary: 0/5
   expect: 181 181(true) 182
   expect: 181 181(false) 183
   expect: 182 181(true) 182
   expect: 182 181(false) 183
   expect: 181(true) 182 181
*/
void
pathcov003e (int a)
/* END */
{
  int x = 0;
  int i = 0;
  while (i++ < a)
    x++;
}

/* BEGIN paths
   summary: 0/2
   expect: 195 198(false) 199
   expect: 198(true) 198
*/
void
pathcov003f (int a)
/* END */
{
  int x = 0;
  int i = 0;
  do {
      x++;
  } while (i++ < a);
}

/* BEGIN paths
   summary: 0/5
   expect: 214 217(true) 221
   expect: 214 217(false) 223
   expect: 217(true) 221 217
   expect: 221 217(true) 221
   expect: 221 217(false) 223
*/
void
pathcov003g (int a)
/* END */
{
  int i = 0;
  int x = 0;

top:
  if (i < a)
    {
      x++;
      i++;
      goto top;
    }
}

/* This example has a good mix of control flow structures which makes it nice
   at identifying problems with the bookeeping/instrumentation.  */

/* BEGIN paths
   summary: 0/9
   expect: 244(false) 248 248(true) 248(true) 250(true) 251 254
   expect: 244(false) 248 248(true) 248(false) 254
   expect: 244(false) 248 248(false) 254
   expect: 244(true) 254
   expect: 250(false) 248(true) 248(true) 250
   expect: 250(false) 248(true) 248(false) 254
   expect: 248(true) 248(true) 250(false) 248
   expect: 248(true) 250(false) 248(true) 248
   expect: 248(true) 250(false) 248(false) 254
*/
void
pathcov004a (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* BEGIN paths
   args: (1, 0, 0, 0)
   summary: 1/9 */
void
pathcov004b (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* BEGIN paths
   args: (0, 0, 0, 0)
   summary: 1/9 */
void
pathcov004c (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* BEGIN paths
   args: (0, 1, 0, 0)
   summary: 1/9 */
void
pathcov004d (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* BEGIN paths
   args: (0, 1, 1, 0)
   summary: 2/9 */
void
pathcov004e (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* BEGIN paths
   args: (0, 2, 1, 0)
   summary: 3/9 */
void
pathcov004f (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* Funny loop exits.  */

/* BEGIN paths
   summary: 0/14
*/
void
pathcov005a (int a, int b, int c)
/* END */
{
  while (a)
    {
      if (b)
	return;
      while (c--)
	a++;
    }
}

void
pathcov005b (int a, int b, int c)
/* END */
{
  if (a)
    goto loop;

  while (b > 0)
    {
      b--;
loop:
      while (c--)
	a++;
    }
}

void
pathcov005c (int a, int b, int c)
/* END */
{
  while (a-- > 0) c++;
  while (b-- > 0) c++;
}

/* BEGIN paths
   summary: 0/67

   This is a sanity check and baseline and should not be executed.

   With >64 cases we should have >64 paths which guarantees we cannot fit the
   full bitset within a in a single gcov type.  We want to only update the
   relevant counters because extra instructions are expensive in compile time
   and binary bloat, and verify that only taken paths are recorded.  */
void
pathcov006a (int a)
/* END */
{
  int x = 0;
  switch (a)
  {
    case 0: x++; break;
    case 1: x++; break;
    case 2: x++; break;
    case 3: x++; break;
    case 4: x++; break;
    case 5: x++; break;
    case 6: x++; break;
    case 7: x++; break;
    case 8: x++; break;
    case 9: x++; break;
    case 10: x++; break;
    case 11: x++; break;
    case 12: x++; break;
    case 13: x++; break;
    case 14: x++; break;
    case 15: x++; break;
    case 16: x++; break;
    case 17: x++; break;
    case 18: x++; break;
    case 19: x++; break;
    case 20: x++; break;
    case 21: x++; break;
    case 22: x++; break;
    case 23: x++; break;
    case 24: x++; break;
    case 25: x++; break;
    case 26: x++; break;
    case 27: x++; break;
    case 28: x++; break;
    case 29: x++; break;
    case 30: x++; break;
    case 31: x++; break;
    case 32: x++; break;
    case 33: x++; break;
    case 34: x++; break;
    case 35: x++; break;
    case 36: x++; break;
    case 37: x++; break;
    case 38: x++; break;
    case 39: x++; break;
    case 40: x++; break;
    case 41: x++; break;
    case 42: x++; break;
    case 43: x++; break;
    case 44: x++; break;
    case 45: x++; break;
    case 46: x++; break;
    case 47: x++; break;
    case 48: x++; break;
    case 49: x++; break;
    case 50: x++; break;
    case 51: x++; break;
    case 52: x++; break;
    case 53: x++; break;
    case 54: x++; break;
    case 55: x++; break;
    case 56: x++; break;
    case 57: x++; break;
    case 58: x++; break;
    case 59: x++; break;
    case 60: x++; break;
    case 61: x++; break;
    case 62: x++; break;
    case 63: x++; break;
    case 64: x++; break;
    case 65: x++; break;
  }
}

/* BEGIN paths
   args: (0)
   summary: 1/67
*/
void
pathcov006b (int a)
/* END */
{
  int x = 0;
  switch (a)
  {
    case 0: x++; break;
    case 1: x++; break;
    case 2: x++; break;
    case 3: x++; break;
    case 4: x++; break;
    case 5: x++; break;
    case 6: x++; break;
    case 7: x++; break;
    case 8: x++; break;
    case 9: x++; break;
    case 10: x++; break;
    case 11: x++; break;
    case 12: x++; break;
    case 13: x++; break;
    case 14: x++; break;
    case 15: x++; break;
    case 16: x++; break;
    case 17: x++; break;
    case 18: x++; break;
    case 19: x++; break;
    case 20: x++; break;
    case 21: x++; break;
    case 22: x++; break;
    case 23: x++; break;
    case 24: x++; break;
    case 25: x++; break;
    case 26: x++; break;
    case 27: x++; break;
    case 28: x++; break;
    case 29: x++; break;
    case 30: x++; break;
    case 31: x++; break;
    case 32: x++; break;
    case 33: x++; break;
    case 34: x++; break;
    case 35: x++; break;
    case 36: x++; break;
    case 37: x++; break;
    case 38: x++; break;
    case 39: x++; break;
    case 40: x++; break;
    case 41: x++; break;
    case 42: x++; break;
    case 43: x++; break;
    case 44: x++; break;
    case 45: x++; break;
    case 46: x++; break;
    case 47: x++; break;
    case 48: x++; break;
    case 49: x++; break;
    case 50: x++; break;
    case 51: x++; break;
    case 52: x++; break;
    case 53: x++; break;
    case 54: x++; break;
    case 55: x++; break;
    case 56: x++; break;
    case 57: x++; break;
    case 58: x++; break;
    case 59: x++; break;
    case 60: x++; break;
    case 61: x++; break;
    case 62: x++; break;
    case 63: x++; break;
    case 64: x++; break;
    case 65: x++; break;
  }
}

/* BEGIN paths
   args: (64)
   summary: 1/67 */
void
pathcov006c (int a)
/* END */
{
  int x = 0;
  switch (a)
  {
    case 0: x++; break;
    case 1: x++; break;
    case 2: x++; break;
    case 3: x++; break;
    case 4: x++; break;
    case 5: x++; break;
    case 6: x++; break;
    case 7: x++; break;
    case 8: x++; break;
    case 9: x++; break;
    case 10: x++; break;
    case 11: x++; break;
    case 12: x++; break;
    case 13: x++; break;
    case 14: x++; break;
    case 15: x++; break;
    case 16: x++; break;
    case 17: x++; break;
    case 18: x++; break;
    case 19: x++; break;
    case 20: x++; break;
    case 21: x++; break;
    case 22: x++; break;
    case 23: x++; break;
    case 24: x++; break;
    case 25: x++; break;
    case 26: x++; break;
    case 27: x++; break;
    case 28: x++; break;
    case 29: x++; break;
    case 30: x++; break;
    case 31: x++; break;
    case 32: x++; break;
    case 33: x++; break;
    case 34: x++; break;
    case 35: x++; break;
    case 36: x++; break;
    case 37: x++; break;
    case 38: x++; break;
    case 39: x++; break;
    case 40: x++; break;
    case 41: x++; break;
    case 42: x++; break;
    case 43: x++; break;
    case 44: x++; break;
    case 45: x++; break;
    case 46: x++; break;
    case 47: x++; break;
    case 48: x++; break;
    case 49: x++; break;
    case 50: x++; break;
    case 51: x++; break;
    case 52: x++; break;
    case 53: x++; break;
    case 54: x++; break;
    case 55: x++; break;
    case 56: x++; break;
    case 57: x++; break;
    case 58: x++; break;
    case 59: x++; break;
    case 60: x++; break;
    case 61: x++; break;
    case 62: x++; break;
    case 63: x++; break;
    case 64: x++; break;
    case 65: x++; break;
  }
}

/* BEGIN paths
   args: (2, 65)
   summary: 2/67

   In this case we should record a path in both halves of the accumulator
   bitsets.  Note that the paths don't overlap with the single-half examples in
   006b and 006c to reduce the chance of accidental passes.  */
void
pathcov006d (int a)
/* END */
{
  int x = 0;
  switch (a)
  {
    case 0: x++; break;
    case 1: x++; break;
    case 2: x++; break;
    case 3: x++; break;
    case 4: x++; break;
    case 5: x++; break;
    case 6: x++; break;
    case 7: x++; break;
    case 8: x++; break;
    case 9: x++; break;
    case 10: x++; break;
    case 11: x++; break;
    case 12: x++; break;
    case 13: x++; break;
    case 14: x++; break;
    case 15: x++; break;
    case 16: x++; break;
    case 17: x++; break;
    case 18: x++; break;
    case 19: x++; break;
    case 20: x++; break;
    case 21: x++; break;
    case 22: x++; break;
    case 23: x++; break;
    case 24: x++; break;
    case 25: x++; break;
    case 26: x++; break;
    case 27: x++; break;
    case 28: x++; break;
    case 29: x++; break;
    case 30: x++; break;
    case 31: x++; break;
    case 32: x++; break;
    case 33: x++; break;
    case 34: x++; break;
    case 35: x++; break;
    case 36: x++; break;
    case 37: x++; break;
    case 38: x++; break;
    case 39: x++; break;
    case 40: x++; break;
    case 41: x++; break;
    case 42: x++; break;
    case 43: x++; break;
    case 44: x++; break;
    case 45: x++; break;
    case 46: x++; break;
    case 47: x++; break;
    case 48: x++; break;
    case 49: x++; break;
    case 50: x++; break;
    case 51: x++; break;
    case 52: x++; break;
    case 53: x++; break;
    case 54: x++; break;
    case 55: x++; break;
    case 56: x++; break;
    case 57: x++; break;
    case 58: x++; break;
    case 59: x++; break;
    case 60: x++; break;
    case 61: x++; break;
    case 62: x++; break;
    case 63: x++; break;
    case 64: x++; break;
    case 65: x++; break;
  }
}

/* BEGIN paths
   args: (66)
   summary: 1/67

   This case should hit the empty default.  */
void
pathcov006e (int a)
/* END */
{
  int x = 0;
  switch (a)
  {
    case 0: x++; break;
    case 1: x++; break;
    case 2: x++; break;
    case 3: x++; break;
    case 4: x++; break;
    case 5: x++; break;
    case 6: x++; break;
    case 7: x++; break;
    case 8: x++; break;
    case 9: x++; break;
    case 10: x++; break;
    case 11: x++; break;
    case 12: x++; break;
    case 13: x++; break;
    case 14: x++; break;
    case 15: x++; break;
    case 16: x++; break;
    case 17: x++; break;
    case 18: x++; break;
    case 19: x++; break;
    case 20: x++; break;
    case 21: x++; break;
    case 22: x++; break;
    case 23: x++; break;
    case 24: x++; break;
    case 25: x++; break;
    case 26: x++; break;
    case 27: x++; break;
    case 28: x++; break;
    case 29: x++; break;
    case 30: x++; break;
    case 31: x++; break;
    case 32: x++; break;
    case 33: x++; break;
    case 34: x++; break;
    case 35: x++; break;
    case 36: x++; break;
    case 37: x++; break;
    case 38: x++; break;
    case 39: x++; break;
    case 40: x++; break;
    case 41: x++; break;
    case 42: x++; break;
    case 43: x++; break;
    case 44: x++; break;
    case 45: x++; break;
    case 46: x++; break;
    case 47: x++; break;
    case 48: x++; break;
    case 49: x++; break;
    case 50: x++; break;
    case 51: x++; break;
    case 52: x++; break;
    case 53: x++; break;
    case 54: x++; break;
    case 55: x++; break;
    case 56: x++; break;
    case 57: x++; break;
    case 58: x++; break;
    case 59: x++; break;
    case 60: x++; break;
    case 61: x++; break;
    case 62: x++; break;
    case 63: x++; break;
    case 64: x++; break;
    case 65: x++; break;
    default:
  }
}

void *alloc (int) { return 0; }
void *getcwd (void *, int) { return 0; }
void release (void *) {}

/* Based on gnu_getcwd from tree.c.  This function crashed in early
   development, and is mostly useful as a regression test.

   BEGIN paths
   summary: 0/8  */
void *gnu_getcwd()
/* END */
{
  int size = 100;
  void *buffer = alloc (size);

  while (1) {
    void *value = getcwd (buffer, size);
    if (value != 0) return buffer;
    size *= 2;
    release (buffer);
    buffer = (void *) alloc (size);
  }
}

/* BEGIN paths
   summary: 0/6  */
void pathcov007a (int a)
/* END */
{
  goto mid;
  while (1)
    {
      a--;
    mid:
      a--;
      if (a < -5)
	break;
      a--;
    }
}

int
main ()
{
  pathcov001a ();
  pathcov001b ();
  /* not called: pathcov001c (); */

  /* not called: pathcov002a (); */
  pathcov002b (0);
  pathcov002c (1);
  pathcov002d (0);
  pathcov002d (1);

  pathcov004b (1, 0, 0, 0);
  pathcov004c (0, 0, 0, 0);
  pathcov004d (0, 1, 0, 0);
  pathcov004e (0, 1, 1, 0);
  pathcov004f (0, 2, 1, 0);

  /* not called: pathcov006a (); */
  pathcov006b (0);
  pathcov006c (64);
  pathcov006d (2);
  pathcov006d (65);
  pathcov006e (66);
}

/* { dg-final { run-gcov prime-paths { --prime-paths-lines gcov-30.c } } } */
