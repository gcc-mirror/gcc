
extern int abs (int);
extern long labs (long);

typedef signed char *__restrict__ pRINT8;
typedef short *__restrict__ pRINT16;
typedef int *__restrict__ pRINT32;
typedef long *__restrict__ pRLONG;
typedef long long *__restrict__ pRINT64;

#define DEF_ABS(size)  void absolute_s##size (pRINT##size a, pRINT##size b) \
		       { \
			 int i;  \
			 for (i=0; i<N; i++)     \
			    a[i] = (b[i] > 0 ? b[i] : -b[i]); \
		       }

DEF_ABS (8);
DEF_ABS (16);
DEF_ABS (32);
DEF_ABS (64);

/* Test abs () vectorization.  */
void absolute_s32_lib (pRINT32 a, pRINT32 b)
{
  int i;
  for (i=0; i<N; i++)
    a[i] = abs (b[i]);
}

void absolute_l32_lib (pRLONG a, pRLONG b)
{
  int i;
  for (i=0; i<N; i++)
    a[i] = labs (b[i]);
}
