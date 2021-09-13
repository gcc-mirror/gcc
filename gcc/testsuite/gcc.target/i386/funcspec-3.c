/* Test whether using target specific options, we can generate popcnt by
   setting the architecture.  */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=k8" } } */
/* { dg-options "-O2 -march=k8 -mno-sse3" } */

extern void exit (int);
extern void abort (void);

#define SSE4A_ATTR __attribute__((__target__("arch=amdfam10")))
#define SSE42_ATTR __attribute__((__target__("sse4.2")))

static int sse4a_pop_i (int a) SSE4A_ATTR;
static long sse42_pop_l (long a) SSE42_ATTR;
static int generic_pop_i (int a);
static long generic_pop_l (long a);

static
int sse4a_pop_i (int a)
{
  return __builtin_popcount (a);
}

static
long sse42_pop_l (long a)
{
  return __builtin_popcountl (a);
}

static
int generic_pop_i (int a)
{
  return __builtin_popcount (a);
}

static
long generic_pop_l (long a)
{
  return __builtin_popcountl (a);
}

int five = 5;
long seven = 7;

int main ()
{
  if (sse4a_pop_i (five) != 2)
    abort ();

  if (sse42_pop_l (seven) != 3L)
    abort ();

  if (generic_pop_i (five) != 2)
    abort ();

  if (generic_pop_l (seven) != 3L)
    abort ();

  exit (0);
}

/* { dg-final { scan-assembler "popcntl" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "popcntq" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times "popcnt" 2 { target *-*-darwin* } } } */
/* { dg-final { scan-assembler "call\t(\[^\n\r\]*)sse4a_pop_i" } } */
/* { dg-final { scan-assembler "call\t(\[^\n\r\]*)sse42_pop_l" } } */
/* { dg-final { scan-assembler "call\t(\[^\n\r\]*)popcountdi2" } } */
