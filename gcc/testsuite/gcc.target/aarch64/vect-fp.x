
typedef float F32;
typedef double F64;
typedef float *__restrict__ pRF32;
typedef double *__restrict__ pRF64;

extern float fabsf (float);
extern double fabs (double);

#define DEF3a(fname, type, op) \
			 void  fname##_##type (pR##type a,   \
					       pR##type b,   \
					       pR##type c)   \
			 {				     \
			   int i;			     \
			   for (i = 0; i < 16; i++)	     \
			     a[i] = op (b[i] - c[i]);	     \
			 }

#define DEF3(fname, type, op) \
			void  fname##_##type (pR##type a,   \
					      pR##type b,   \
					      pR##type c)   \
			{				    \
			  int i; 			    \
			  for (i = 0; i < 16; i++)	    \
			    a[i] = b[i] op c[i];	    \
			}

#define DEF2(fname, type, op) \
			void fname##_##type (pR##type a, \
					     pR##type b) \
			{				  \
			  int i; 			  \
			  for (i = 0; i < 16; i++)	  \
			    a[i] = op(b[i]);		  \
			}


#define DEFN3a(fname, op) \
		 DEF3a (fname, F32, op) \
		 DEF3a (fname, F64, op)

#define DEFN3(fname, op) \
		DEF3 (fname, F32, op) \
		DEF3 (fname, F64, op)

#define DEFN2(fname, op) \
		DEF2 (fname, F32, op) \
		DEF2 (fname, F64, op)

DEFN3 (add, +)
DEFN3 (sub, -)
DEFN3 (mul, *)
DEFN3 (div, /)
DEFN2 (neg, -)
DEF2 (abs, F32, fabsf)
DEF2 (abs, F64, fabs)
DEF3a (fabd, F32, fabsf)
DEF3a (fabd, F64, fabs)
