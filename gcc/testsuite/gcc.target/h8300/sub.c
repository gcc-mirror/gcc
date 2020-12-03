/* { dg-do compile }  */
/* { dg-options "-mh -mint32 -O2" }  */
/* { dg-final { scan-assembler-not "cmp" } }  */

typedef unsigned char uchar;
typedef signed char schar;
typedef unsigned short ushort;
typedef unsigned long ulong;

volatile void abort (void);


#define SUB(T)\
T subE##T (T x, T y) { T t = x - y ; if (t == 0) abort (); return t; } \
T subNE##T (T x, T y) { T t = x - y ; if (t != 0) return t; abort (); } \
T subGE##T (T x, T y) { T t = x - y ; if (t >= 0) abort (); return t; } \
T subLT##T (T x, T y) { T t = x - y ; if (t < 0) abort (); return t; }

#define SUBC(T,N)\
T subEQ##N##T (T a) { T t = a - N; if (t == 0) abort (); return t; } \
T subNE##N##T (T a) { T t = a - N; if (t != 0) return t; abort (); } \
T subGE##N##T (T a) { T t = a - N; if (t >= 0) abort (); return t; } \
T subLT##N##T (T a) { T t = a - N; if (t < 0) abort (); return t; }

#define SUBNC(T,N)\
T subEQN##N##T (T a) { T t = a - -N; if (t == 0) abort (); return t; } \
T subNEN##N##T (T a) { T t = a - -N; if (t != 0) return t; abort (); } \
T subGEN##N##T (T a) { T t = a - -N; if (t >= 0) abort (); return t; } \
T subLTN##N##T (T a) { T t = a - -N; if (t < 0) abort (); return t; }


SUB (schar)
SUB (short)
SUB (long)
SUB (uchar)
SUB (ushort)
SUB (ulong)



SUBC (schar,1)
SUBC (schar,2)
SUBC (schar,3)
SUBC (schar,4)
SUBC (schar,6)
SUBC (schar,8)
SUBNC (schar,1)
SUBNC (schar,2)
SUBNC (schar,3)
SUBNC (schar,4)
SUBNC (schar,6)
SUBNC (schar,8)

SUBC (uchar,1)
SUBC (uchar,2)
SUBC (uchar,3)
SUBC (uchar,4)
SUBC (uchar,6)
SUBC (uchar,8)
SUBNC (uchar,1)
SUBNC (uchar,2)
SUBNC (uchar,3)
SUBNC (uchar,4)
SUBNC (uchar,6)
SUBNC (uchar,8)

SUBC (short,1)
SUBC (short,2)
SUBC (short,3)
SUBC (short,4)
SUBC (short,6)
SUBC (short,8)
SUBNC (short,1)
SUBNC (short,2)
SUBNC (short,3)
SUBNC (short,4)
SUBNC (short,6)
SUBNC (short,8)

SUBC (ushort,1)
SUBC (ushort,2)
SUBC (ushort,3)
SUBC (ushort,4)
SUBC (ushort,6)
SUBC (ushort,8)
SUBNC (ushort,1)
SUBNC (ushort,2)
SUBNC (ushort,3)
SUBNC (ushort,4)
SUBNC (ushort,6)
SUBNC (ushort,8)

SUBC (long,1)
SUBC (long,2)
SUBC (long,3)
SUBC (long,4)
SUBC (long,6)
SUBC (long,8)
SUBNC (long,1)
SUBNC (long,2)
SUBNC (long,3)
SUBNC (long,4)
SUBNC (long,6)
SUBNC (long,8)

SUBC (ulong,1)
SUBC (ulong,2)
SUBC (ulong,3)
SUBC (ulong,4)
SUBC (ulong,6)
SUBC (ulong,8)
SUBNC (ulong,1)
SUBNC (ulong,2)
SUBNC (ulong,3)
SUBNC (ulong,4)
SUBNC (ulong,6)
SUBNC (ulong,8)

