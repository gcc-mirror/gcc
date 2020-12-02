/* { dg-do compile }  */
/* { dg-options "-mh -mint32 -O2" }  */
/* { dg-final { scan-assembler-not "cmp" } }  */

typedef unsigned char uchar;
typedef signed char schar;
typedef unsigned short ushort;
typedef unsigned long ulong;

volatile void abort (void);


#define ADD(T)\
T addE##T (T x, T y) { T t = x + y ; if (t == 0) abort (); return t; } \
T addNE##T (T x, T y) { T t = x + y ; if (t != 0) return t; abort (); } \
T addGE##T (T x, T y) { T t = x + y ; if (t >= 0) abort (); return t; } \
T addLT##T (T x, T y) { T t = x + y ; if (t < 0) abort (); return t; }

#define ADDC(T,N)\
T addEQ##N##T (T a) { T t = a + N; if (t == 0) abort (); return t; } \
T addNE##N##T (T a) { T t = a + N; if (t != 0) return t; abort (); } \
T addGE##N##T (T a) { T t = a + N; if (t >= 0) abort (); return t; } \
T addLT##N##T (T a) { T t = a + N; if (t < 0) abort (); return t; }

#define ADDNC(T,N)\
T addEQN##N##T (T a) { T t = a + -N; if (t == 0) abort (); return t; } \
T addNEN##N##T (T a) { T t = a + -N; if (t != 0) return t; abort (); } \
T addGEN##N##T (T a) { T t = a + -N; if (t >= 0) abort (); return t; } \
T addLTN##N##T (T a) { T t = a + -N; if (t < 0) abort (); return t; }


ADD (schar)
ADD (short)
ADD (long)
ADD (uchar)
ADD (ushort)
ADD (ulong)



ADDC (schar,1)
ADDC (schar,2)
ADDC (schar,3)
ADDC (schar,4)
ADDC (schar,6)
ADDC (schar,8)
ADDNC (schar,1)
ADDNC (schar,2)
ADDNC (schar,3)
ADDNC (schar,4)
ADDNC (schar,6)
ADDNC (schar,8)

ADDC (uchar,1)
ADDC (uchar,2)
ADDC (uchar,3)
ADDC (uchar,4)
ADDC (uchar,6)
ADDC (uchar,8)
ADDNC (uchar,1)
ADDNC (uchar,2)
ADDNC (uchar,3)
ADDNC (uchar,4)
ADDNC (uchar,6)
ADDNC (uchar,8)

ADDC (short,1)
ADDC (short,2)
ADDC (short,3)
ADDC (short,4)
ADDC (short,6)
ADDC (short,8)
ADDNC (short,1)
ADDNC (short,2)
ADDNC (short,3)
ADDNC (short,4)
ADDNC (short,6)
ADDNC (short,8)

ADDC (ushort,1)
ADDC (ushort,2)
ADDC (ushort,3)
ADDC (ushort,4)
ADDC (ushort,6)
ADDC (ushort,8)
ADDNC (ushort,1)
ADDNC (ushort,2)
ADDNC (ushort,3)
ADDNC (ushort,4)
ADDNC (ushort,6)
ADDNC (ushort,8)

ADDC (long,1)
ADDC (long,2)
ADDC (long,3)
ADDC (long,4)
ADDC (long,6)
ADDC (long,8)
ADDNC (long,1)
ADDNC (long,2)
ADDNC (long,3)
ADDNC (long,4)
ADDNC (long,6)
ADDNC (long,8)

ADDC (ulong,1)
ADDC (ulong,2)
ADDC (ulong,3)
ADDC (ulong,4)
ADDC (ulong,6)
ADDC (ulong,8)
ADDNC (ulong,1)
ADDNC (ulong,2)
ADDNC (ulong,3)
ADDNC (ulong,4)
ADDNC (ulong,6)
ADDNC (ulong,8)

