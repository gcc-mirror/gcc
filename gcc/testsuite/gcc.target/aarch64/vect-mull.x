
typedef signed char *__restrict__ pRSINT8;
typedef signed short *__restrict__ pRSINT16;
typedef signed int *__restrict__ pRSINT32;
typedef signed long long *__restrict__ pRSINT64;

typedef unsigned char *__restrict__ pRUINT8;
typedef unsigned short *__restrict__ pRUINT16;
typedef unsigned int *__restrict__ pRUINT32;
typedef unsigned long long *__restrict__ pRUINT64;

typedef signed short SH;
typedef unsigned short UH;
typedef signed int SS;
typedef unsigned int US;
typedef signed long long SLL;
typedef unsigned long long ULL;

#define DEF_MULLB(sign)   \
		   void widen_mult_##sign##b  (pR##sign##INT##16 a, \
					       pR##sign##INT##8 b, \
					       pR##sign##INT##8 c) \
		   { \
			int i;  \
			for (i=0; i<N; i++)     \
			  a[i] = (sign##H)b[i] * c[i];   \
		   }

#define DEF_MULLH(sign)   \
		   void widen_mult_##sign##h (pR##sign##INT##32 a, \
					      pR##sign##INT##16 b, \
					      pR##sign##INT##16 c) \
		   { \
			int i;  \
			for (i=0; i<N; i++)     \
			  a[i] = (sign##S)b[i] * c[i];   \
		   }
#define DEF_MULLS(sign)   \
		   void widen_mult_##sign##s (pR##sign##INT##64 a, \
					      pR##sign##INT##32 b, \
					      pR##sign##INT##32 c) \
		   { \
			int i;  \
			for (i=0; i<N; i++)     \
			  a[i] = (sign##LL)b[i] * c[i];   \
		   }

#define DEF_MULL2(x) x (S) \
		     x (U)
