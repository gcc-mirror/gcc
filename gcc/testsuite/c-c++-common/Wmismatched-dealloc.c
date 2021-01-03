/* PR middle-end/98166: bogus -Wmismatched-dealloc on user-defined allocator
   and inlining
   { dg-do compile }
   { dg-options "-O2 -Wall" } */


void dealloc_shrt (short *p)
{
  /* A positive offset would be diagnosed but a negative one must
     not be.  */
  __builtin_free (p - 1);       // { dg-bogus "-Wmismatched-dealloc" }
}

__attribute__ ((malloc (dealloc_shrt)))
short* alloc_shrt (int n) /* { return malloc (n) + 1; } */;

void test_nowarn_shrt (int n)
{
  short *p = alloc_shrt (n);
  dealloc_shrt (p);
}


void dealloc_int (int *p) /* { free (p - 1); } */;

__attribute__ ((malloc (dealloc_int, 1)))
int* alloc_int (int n)
{
  return (int*)__builtin_malloc (n) + 1;
}

void test_nowarn_int (int n)
{
  int *p = alloc_int (n);
  dealloc_int (p);              // { dg-bogus "-Wmismatched-dealloc" }
}


void dealloc_long (int, long *p) /* { free (p - 2); } */;

__attribute__ ((malloc (dealloc_long, 2)))
inline long*
alloc_long (int n) {            // { dg-warning "'malloc \\(\[^\n\r\]*dealloc_long\[^\n\r\]*\\)' attribute ignored on functions declared 'inline'" }
  return (long*)__builtin_malloc (n) + 2;
}

void test_nowarn_long (int n)
{
  long *p = alloc_long (n);
  dealloc_long (0, p);          // { dg-bogus "\\\[-Wmismatched-dealloc" }
}


inline void
dealloc_float (int, int, float *p)  // { dg-message "deallocation function declared here" }
{
  __builtin_free (p - 3);
}

__attribute__ ((malloc (dealloc_float, 3)))
float* alloc_float (int n);     // { dg-warning "'malloc \\(\[^\n\r\]*dealloc_float\[^\n\r\]*\\)' attribute ignored with deallocation functions declared 'inline'" }

void test_nowarn_float (int n)
{
  float *p = alloc_float (n);
  dealloc_float (0, 1, p);      // { dg-bogus "\\\[-Wmismatched-dealloc" }
}
