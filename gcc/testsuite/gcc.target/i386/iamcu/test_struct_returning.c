/* This tests returning of structures.  */

#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

int current_test;
int num_failed = 0;

typedef enum {
  EAX = 0,
  EAX_EDX,
  LONG_LONG,
  FLOAT,
  DOUBLE,
  FLOAT_FLOAT,
  EAX_FLOAT,
  FLOAT_EDX,
  MEM
} Type;

/* Structures which should be returned in EAX/LONG_LONG/EAX_EDX.  */
#define D(I,MEMBERS,C,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = C; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; iamcu_memset (&s, 0, sizeof(s)); B; return s; }

D(1,char m1, EAX, s.m1=42)
D(2,short m1, EAX, s.m1=42)
D(3,int m1, EAX, s.m1=42)
D(4,char m1[3], EAX, s.m1[0]=42)
D(5,char m1[4], EAX, s.m1[0]=42)
D(6,char m1;char m2; char m3, EAX, s.m1=42)
D(7,char m1;short m2, EAX, s.m1=42)

D(30,long long m1, LONG_LONG, s.m1=0xadadbeefadadbeefLL)

D(50,short m1;int m2, EAX_EDX, s.m1=42; s.m2=43)
D(51,char m1;int m2, EAX_EDX, s.m1=42; s.m2=43)
D(52,char m1[5], EAX_EDX, s.m1[0]=42; s.m1[4]=43)
D(53,char m1[6], EAX_EDX, s.m1[0]=42; s.m1[4]=43)
D(54,char m1[7], EAX_EDX, s.m1[0]=42; s.m1[4]=43)
D(55,char m1[8], EAX_EDX, s.m1[0]=42; s.m1[4]=43)
D(56,char m1;short m2[2], EAX_EDX, s.m1=42; s.m2[1]=43)
D(57,short m1[4], EAX_EDX, s.m1[0]=42; s.m1[2]=43)
D(58,int m1[2], EAX_EDX, s.m1[0]=42; s.m1[1]=43)
D(59,int m1;char m2, EAX_EDX, s.m1=42; s.m2=43)
D(60,int m1;short m2, EAX_EDX, s.m1=42; s.m2=43)
D(61,int m1;short m2; char m3, EAX_EDX, s.m1=42; s.m2=43)
D(62,int m1;char m2; short m3, EAX_EDX, s.m1=42; s.m2=43)

/* Packed members.  */
D(100,short m1[1];int m2 PACKED, EAX_EDX, s.m1[0]=42; s.m2=43)
D(101,char m1; short m2 PACKED; char m3, EAX_EDX, s.m1=42; s.m3=43)

/* Structures which should be returned in FLOAT/DOUBLE.  */
#undef D
#define D(I,MEMBERS,C,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = C; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; iamcu_memset (&s, 0, sizeof(s)); B; return s; }

D(200,float f, FLOAT, s.f=42)
D(201,double d, DOUBLE, s.d=42)

D(300,float m;char m2, FLOAT_EDX, s.m=42; s.m2=43)
D(301,float m;short m2, FLOAT_EDX, s.m=42; s.m2=43)
D(302,float m;int m2, FLOAT_EDX, s.m=42; s.m2=43)

D(400,char m1; float m2, EAX_FLOAT, s.m1=42; s.m2=43)
D(401,short m1; float m2, EAX_FLOAT, s.m1=42; s.m2=43)
D(402,int m1; float m2, EAX_FLOAT, s.m1=42; s.m2=43)

D(500,float m;float m2, FLOAT_FLOAT, s.m=42; s.m2=43)
D(501,float f[2], FLOAT, s.f[0]=42; s.f[1]=43)

/* Structures which should be returned in MEM.  */
void *struct_addr;
#undef D
#define D(I,MEMBERS) struct S_ ## I { MEMBERS ; }; Type class_ ## I = MEM; \
struct S_ ## I f_ ## I (void) { union {unsigned char c; struct S_ ## I s;} u; iamcu_memset (&u.s, 0, sizeof(u.s)); u.c = 42; return u.s; }

/* Too large.  */
D(600,char m1[17])
D(601,short m1[9])
D(602,int m1[5])
D(603,long m1[3])
D(604,short m1[8];char c)
D(605,char m1[1];int i[4])
D(606,float m1[5])
D(607,double m1[3])
D(608,char m1[1];float f[4])
D(609,char m1[1];double d[2])
D(610,__complex long double m1[1])

/* Too large due to padding.  */
D(611,char m1[1]; int i; char c2)

/* Special tests.  */
#undef D
#define D(I,MEMBERS,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = MEM; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; B; return s; }
D(700,float f[4], s.f[0] = s.f[1] = s.f[2] = s.f[3] = 42)

void
check_eax (void)
{
  switch (current_test)
    {
    case 1:
    case 4:
    case 5:
    case 6:
    case 7:
      eax &= 0xff;
      break;
    case 2:
      eax &= 0xffff;
      break;
    case 3:
      eax &= 0xffff;
      break;
    default:
      abort ();
    }
  if (eax != 42)
    num_failed++;
}

void
check_eax_edx (void)
{
  unsigned long long ll = eax | ((unsigned long long) edx) << 32;
  switch (current_test)
    {
    case 50:
      eax &= 0xffff;
      break;
    case 52:
    case 53:
    case 54:
    case 55:
      edx &= 0xff;
    case 51:
      eax &= 0xff;
      break;
    case 56:
      eax &= 0xff;
      edx &= 0xffff;
      break;
    case 57:
      eax &= 0xffff;
      edx &= 0xffff;
      break;
    case 58:
      break;
    case 59:
    case 62:
      edx &= 0xff;
      break;
    case 60:
    case 61:
      edx &= 0xffff;
      break;
    case 100:
      eax &= 0xffff;
      edx = (ll >> 16) & 0xffffffff;
      break;
    case 101:
      edx = (eax >> 24) & 0xff;
      eax &= 0xff;
      break;
    default:
      abort ();
    }
  if (eax != 42 || edx != 43)
    num_failed++;
}

void
check_float_edx (void)
{
  union
    {
      unsigned long l;
      float f;
    } ueax;
  switch (current_test)
    {
    case 300:
      edx &= 0xff;
      break;
    case 301:
      edx &= 0xffff;
      break;
    case 302:
      edx &= 0xffff;
      break;
    default:
      abort ();
    }
  ueax.l = eax;
  if (ueax.f != 42 || edx != 43)
    num_failed++;
}

void
check_eax_float (void)
{
  union
    {
      unsigned long l;
      float f;
    } uedx;
  switch (current_test)
    {
    case 400:
      eax &= 0xff;
      break;
    case 401:
      eax &= 0xffff;
      break;
    case 402:
      eax &= 0xffff;
      break;
    default:
      abort ();
    }
  uedx.l = edx;
  if (eax != 42 || uedx.f != 43)
    num_failed++;
}

void
check_float_float (void)
{
  union
    {
      unsigned long l;
      float f;
    } ueax, uedx;
  switch (current_test)
    {
    case 500:
    case 501:
      break;
    default:
      abort ();
    }
  ueax.l = eax;
  uedx.l = edx;
  if (ueax.f != 42 || uedx.f != 43)
    num_failed++;
}

void
check_all (Type class, unsigned long size)
{
  union
    {
      struct
	{
	  unsigned long eax;
	  unsigned long edx;
	} eax_edx;
      unsigned long long ll;
      float f;
      double d;
    } u;

  switch (class)
    {
    case EAX:
      check_eax ();
      break;
    case LONG_LONG:
      if (0xadadbeefL != eax || 0xadadbeefL != edx)
	num_failed++;
      break;
    case EAX_EDX:
      check_eax_edx ();
      break;
    case FLOAT:
      u.eax_edx.eax = eax;
      if (u.f != 42)
	num_failed++;
      break;
    case DOUBLE:
      u.eax_edx.eax = eax;
      u.eax_edx.edx = edx;
      if (u.d != 42)
	num_failed++;
      break;
    case FLOAT_EDX:
      check_float_edx ();
      break;
    case FLOAT_FLOAT:
      check_float_float ();
      break;
    case EAX_FLOAT:
      check_eax_float ();
      break;
    case MEM:
      /* sret_eax contains a slot whose address is given to the f_*
	 functions.  The slot may be a temporary one on stack.  When
	 this function is called, hopefully this slot hasn't be
	 overriden.  */
      if (sret_eax != eax)
	num_failed++;
      else if (current_test < 700)
	{
	  if (*(unsigned char*)sret_eax != 42
	      || *(unsigned char*)struct_addr != 42)
	    num_failed++;
	}
      else
	{
	  if (*(float *)sret_eax != 42
	      || *(float *)struct_addr != 42)
	    num_failed++;
	}
      break;
    }
}

#undef D
#define D(I) { static struct S_ ## I s; current_test = I; struct_addr = (void*)&s; \
  clear_non_sret_int_registers; \
  s = WRAP_RET(f_ ## I) (); \
  check_all(class_ ## I, sizeof(s)); \
}

int
main (void)
{
  D(1) D(2) D(3) D(4) D(5) D(6) D(7)

  D(30)

  D(50) D(51) D(52) D(53) D(54) D(55) D(56) D(57) D(58) D(59)
  D(60) D(61) D(62)

  D(100) D(101)

  D(200) D(201)

  D(300) D(301) D(302)

  D(400) D(401) D(402)

  D(500) D(501)

  D(600) D(601) D(602) D(603) D(604) D(605) D(606) D(607) D(608) D(609)
  D(610) D(611)

  D(700)

  if (num_failed)
    abort ();

  return 0;
}
