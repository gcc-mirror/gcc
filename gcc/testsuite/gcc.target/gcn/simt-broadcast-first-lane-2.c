/* Check that the generic __builtin_gcn_simt_broadcast_first_lane returns a
   value of its argument's type, and gives every active lane the first active
   lane's value, for integers of every size, an enum, a pointer and
   floating-point values.  The odd lanes check the case where the first
   active lane is not lane 0.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)
#define BCAST __builtin_gcn_simt_broadcast_first_lane
#define SAME_TYPE(X, T) \
  __builtin_types_compatible_p (__typeof__ (BCAST ((X))), T)

enum e { E0, E1 = 1000 };

/* Global variables aren't thread-local.  */
signed char c[64];
unsigned short s[64];
unsigned int u[64];
long l[64];
unsigned long ul[64];
enum e en[64];
int *p[64];
float fl[64];
double db[64];

int in[64];

/* The result types match the argument types.  */
int types_ok = (SAME_TYPE ((signed char) 1, signed char)
		&& SAME_TYPE ((unsigned short) 1, unsigned short)
		&& SAME_TYPE (1u, unsigned int)
		&& SAME_TYPE (1L, long)
		&& SAME_TYPE (1UL, unsigned long)
		&& SAME_TYPE ((enum e) E1, enum e)
		&& SAME_TYPE ((const int *) 0, const int *)
		&& SAME_TYPE (1.0f, float)
		&& SAME_TYPE (1.0, double));

void __attribute__((noclone,noinline))
f1 (int i)
{
  int t = TID;
  c[t] = BCAST ((signed char) (-100 + i));
  s[t] = BCAST ((unsigned short) (60000 + i));
  u[t] = BCAST (0x80000000u + i);
  l[t] = BCAST (((long) (i + 7) << 32) | (0x80000000u + i));
  ul[t] = BCAST (~0UL - i);
  en[t] = BCAST ((enum e) (E1 + i));
  p[t] = BCAST (&in[i]);
  fl[t] = BCAST (-1.5f * (i + 1));
  db[t] = BCAST (1e300 * (i + 1));
}

void __attribute__((noclone,noinline))
f ()
{
  int i = TID;
  if (i & 1)
    f1 (i);
  else
    f1 (0);
}

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  if (!types_ok)
    __builtin_abort ();

  A = __builtin_gcn_simt_enter (1, 2);
  f ();
  __builtin_gcn_simt_exit (A);

  for (i = 0; i < 64; i++)
    {
      /* Even lanes broadcast lane 0's values, and odd lanes lane 1's.  */
      int j = i & 1;
      if (c[i] != -100 + j
	  || s[i] != 60000 + j
	  || u[i] != 0x80000000u + j
	  || l[i] != (((long) (j + 7) << 32) | (0x80000000u + j))
	  || ul[i] != ~0UL - j
	  || en[i] != E1 + j
	  || p[i] != &in[j]
	  || fl[i] != -1.5f * (j + 1)
	  || db[i] != 1e300 * (j + 1))
	__builtin_abort ();
    }

  return 0;
}
