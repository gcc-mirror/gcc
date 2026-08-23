/* The complement spelling of the narrow-clip idiom is only a clamp when the
   comparison that selects the saturated arm is unsigned.  With a signed
   comparison a negative value takes the fall-through arm and keeps its low
   bits, so it must not be turned into a saturating truncation.  */

/* { dg-do run } */
/* { dg-options "-O1 -ftree-vectorize -fdump-tree-vect-details" } */

typedef __UINT16_TYPE__ u16;
typedef __INT32_TYPE__ i32;

#define N 32

static i32 a[N];
static u16 r[N];

__attribute__((noipa)) static void
clip (u16 *__restrict d, const i32 *__restrict s, int n)
{
  for (int i = 0; i < n; i++)
    {
      i32 v = s[i];
      d[i] = v > 65535 ? (~v) >> 31 : v;
    }
}

int
main (void)
{
  for (int i = 0; i < N; i++)
    a[i] = -1;

  clip (r, a, N);

  for (int i = 0; i < N; i++)
    if (r[i] != (u16) -1)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "sat_trunc pattern recognized" "vect" } } */
