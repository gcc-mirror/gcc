#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../../gcc.dg/compat/compat-common.h"

#ifndef SKIP_ATTRIBUTE
# include "../../gcc.dg/compat/vector-defs.h"
#else
typedef int qi;
typedef int hi;
typedef int si;
typedef int di;
typedef float sf;
typedef float df;
typedef int v8qi;
typedef int v16qi;
typedef int v2hi;
typedef int v4hi;
typedef int v8hi;
typedef int v2si;
typedef int v4si;
typedef int v1di;
typedef int v2di;
typedef int v2sf;
typedef int v4sf;
typedef int v16sf;
typedef int v2df;
typedef int u8qi;
typedef int u16qi;
typedef int u2hi;
typedef int u4hi;
typedef int u8hi;
typedef int u2si;
typedef int u4si;
typedef int u1di;
typedef int u2di;
typedef int u2sf;
typedef int u4sf;
typedef int u16sf;
typedef int u2df;
#endif
#if (defined __i386__ || defined __x86_64__) && !defined SKIP_ATTRIBUTE
# ifdef __MMX__
typedef int __m64 __attribute__ ((__vector_size__ (8)));
# else
typedef int __m64;
# endif
# ifdef __SSE__
typedef float __m128 __attribute__ ((__vector_size__ (16)));
# else
typedef int __m128;
# endif
#else
typedef int __m64;
typedef int __m128;
#endif
#if defined (__uClinux__)
/* The maximum achievable alignment on uClinux is usually 16 bytes.  */
#define v16sf v4sf
#define u16sf u4sf
#endif

#define FLDS_MAX	32
extern struct Info
{
  int nfields, nbitfields;
  void *sp, *a0p, *a3p;
  void *flds[FLDS_MAX];
  size_t sz, sizes[FLDS_MAX];
  size_t als, ala0, ala3, aligns[FLDS_MAX];
} info;

extern int intarray[256];
extern int fn0 (void), fn1 (void), fn2 (void), fn3 (void), fn4 (void);
extern int fn5 (void), fn6 (void), fn7 (void), fn8 (void), fn9 (void);

#ifdef DBG
#define FAIL(n, m) printf ("fail %d.%d\n", n, m), ++fails
#else
#define FAIL(n, m) ++fails
#endif

#ifdef SKIP_ATTRIBUTE
# define __attribute__(x)
#endif
#define atal		__attribute__((aligned))
#define atpa		__attribute__((packed))
#define atalpa		__attribute__((aligned, packed))
#define atpaal		__attribute__((packed, aligned))
#define atal1		__attribute__((aligned (1)))
#define atal2		__attribute__((aligned (2)))
#define atal4		__attribute__((aligned (4)))
#define atal8		__attribute__((aligned (8)))
#define atal16		__attribute__((aligned (16)))
#define atal1pa		__attribute__((aligned (1), packed))
#define atal2pa		__attribute__((aligned (2), packed))
#define atal4pa		__attribute__((aligned (4), packed))
#define atal8pa		__attribute__((aligned (8), packed))
#define atal16pa	__attribute__((aligned (16), packed))
#define atpaal1		__attribute__((packed, aligned (1)))
#define atpaal2		__attribute__((packed, aligned (2)))
#define atpaal4		__attribute__((packed, aligned (4)))
#define atpaal8		__attribute__((packed, aligned (8)))
#define atpaal16	__attribute__((packed, aligned (16)))

#if UCHAR_MAX == 255 && USHORT_MAX == 65535 && UINT_MAX == 4294967295U \
    && ULLONG_MAX == 18446744073709551615ULL
/* For ILP32 and LP64 targets, assume float is at least 32-bit
   and double plus long double at least 64-bit.  */
# define atalx1		atal1
# define atalx2		atal2
# define atalx4		atal4
# define atalx8		atal8
# define atalx16	atal16
# define atalx1pa	atal1pa
# define atalx2pa	atal2pa
# define atalx4pa	atal4pa
# define atalx8pa	atal8pa
# define atalx16pa	atal16pa
# define atpaalx1	atpaal1
# define atpaalx2	atpaal2
# define atpaalx4	atpaal4
# define atpaalx8	atpaal8
# define atpaalx16	atpaal16
# if ULONG_MAX > 4294967295UL
#  define ataly8	atal8
#  define ataly8pa	atal8pa
#  define atpaaly8	atpaal8
#  define ataly16	atal16
#  define ataly16pa	atal16pa
#  define atpaaly16	atpaal16
# else
#  define ataly8
#  define ataly8pa
#  define atpaaly8
#  define ataly16
#  define ataly16pa
#  define atpaaly16
# endif
#else
# define atalx1
# define atalx2
# define atalx4
# define atalx8
# define atalx16
# define atalx1pa
# define atalx2pa
# define atalx4pa
# define atalx8pa
# define atalx16pa
# define atpaalx1
# define atpaalx2
# define atpaalx4
# define atpaalx8
# define atpaalx16
# define ataly8
# define ataly8pa
# define atpaaly8
# define ataly16
# define ataly16pa
# define atpaaly16
#endif

#define atQI		__attribute__((mode (QI)))
#define atHI		__attribute__((mode (HI)))
#define atSI		__attribute__((mode (SI)))
#define atDI		__attribute__((mode (DI)))

enum E0 { e0_0 };
enum E1 { e1_0, e1_1 };
enum E2 { e2_m3 = -3, e2_m2, e2_m1, e2_0, e2_1, e2_2, e2_3 };
enum E3 { e3_m127 = -127, e3_m126, e3_m125, e3_0 = 0, e3_125 = 125, e3_126, e3_127 };
enum E4 { e4_0, e4_1, e4_2, e4_3, e4_253 = 253, e4_254, e4_255 };
enum E5 { e5_m32767 = -32767, e5_m32766, e5_m32765, e5_0 = 0, e5_32765 = 32765, e5_32766, e5_32767 };
enum E6 { e6_0, e6_1, e6_2, e6_3, e6_65533 = 65533, e6_65534, e6_65535 };
enum E7 { e7_m2147483647 = -2147483647, e7_m2147483646, e7_m2147483645,
	  e7_0, e7_2147483645 = 2147483645, e7_2147483646, e7_2147483647 };
enum E8 { e8_0, e8_1, e8_2, e8_3, e8_4294967293 = 4294967293U, e8_4294967294, e8_4294967295 };
enum E9 { e9_m1099511627775 = -1099511627775LL, e9_m1099511627774, e9_m1099511627773,
	  e9_0, e9_1099511627773 = 1099511627773LL, e9_1099511627774, e9_1099511627775 };

typedef char Tchar;
typedef signed char Tschar;
typedef unsigned char Tuchar;
typedef short int Tshort;
typedef unsigned short int Tushort;
typedef int Tint;
typedef unsigned int Tuint;
typedef long int Tlong;
typedef unsigned long int Tulong;
typedef long long int Tllong;
typedef unsigned long long int Tullong;
typedef float Tfloat;
typedef double Tdouble;
typedef long double Tldouble;
typedef _Complex float Tcfloat;
typedef _Complex double Tcdouble;
typedef _Complex long double Tcldouble;
typedef bool Tbool;
typedef enum E0 TE0;
typedef enum E1 TE1;
typedef enum E2 TE2;
typedef enum E3 TE3;
typedef enum E4 TE4;
typedef enum E5 TE5;
typedef enum E6 TE6;
typedef enum E7 TE7;
typedef enum E8 TE8;
typedef enum E9 TE9;
typedef void *Tptr;
typedef char *Tcptr;
typedef int *Tiptr;
typedef char Talchar atal;
typedef signed char Talschar atal;
typedef unsigned char Taluchar atal;
typedef short int Talshort atal;
typedef unsigned short int Talushort atal;
typedef int Talint atal;
typedef unsigned int Taluint atal;
typedef long int Tallong atal;
typedef unsigned long int Talulong atal;
typedef long long int Talllong atal;
typedef unsigned long long int Talullong atal;
typedef float Talfloat atal;
typedef double Taldouble atal;
typedef long double Talldouble atal;
typedef _Complex float Talcfloat atal;
typedef _Complex double Talcdouble atal;
typedef _Complex long double Talcldouble atal;
typedef bool Talbool atal;
typedef enum E0 TalE0 atal;
typedef enum E1 TalE1 atal;
typedef enum E2 TalE2 atal;
typedef enum E3 TalE3 atal;
typedef enum E4 TalE4 atal;
typedef enum E5 TalE5 atal;
typedef enum E6 TalE6 atal;
typedef enum E7 TalE7 atal;
typedef enum E8 TalE8 atal;
typedef enum E9 TalE9 atal;
typedef void *Talptr atal;
typedef char *Talcptr atal;
typedef int *Taliptr atal;
typedef char Tal1char atal1;
typedef signed char Tal1schar atal1;
typedef unsigned char Tal1uchar atal1;
typedef short int Tal1short atal1;
typedef unsigned short int Tal1ushort atal1;
typedef int Tal1int atal1;
typedef unsigned int Tal1uint atal1;
typedef long int Tal1long atal1;
typedef unsigned long int Tal1ulong atal1;
typedef long long int Tal1llong atal1;
typedef unsigned long long int Tal1ullong atal1;
typedef float Tal1float atal1;
typedef double Tal1double atal1;
typedef long double Tal1ldouble atal1;
typedef _Complex float Tal1cfloat atal1;
typedef _Complex double Tal1cdouble atal1;
typedef _Complex long double Tal1cldouble atal1;
typedef bool Tal1bool atal1;
typedef enum E0 Tal1E0 atal1;
typedef enum E1 Tal1E1 atal1;
typedef enum E2 Tal1E2 atal1;
typedef enum E3 Tal1E3 atal1;
typedef enum E4 Tal1E4 atal1;
typedef enum E5 Tal1E5 atal1;
typedef enum E6 Tal1E6 atal1;
typedef enum E7 Tal1E7 atal1;
typedef enum E8 Tal1E8 atal1;
typedef enum E9 Tal1E9 atal1;
typedef void *Tal1ptr atal1;
typedef char *Tal1cptr atal1;
typedef int *Tal1iptr atal1;
typedef char Tal2char atal2;
typedef signed char Tal2schar atal2;
typedef unsigned char Tal2uchar atal2;
typedef short int Tal2short atal2;
typedef unsigned short int Tal2ushort atal2;
typedef int Tal2int atal2;
typedef unsigned int Tal2uint atal2;
typedef long int Tal2long atal2;
typedef unsigned long int Tal2ulong atal2;
typedef long long int Tal2llong atal2;
typedef unsigned long long int Tal2ullong atal2;
typedef float Tal2float atal2;
typedef double Tal2double atal2;
typedef long double Tal2ldouble atal2;
typedef _Complex float Tal2cfloat atal2;
typedef _Complex double Tal2cdouble atal2;
typedef _Complex long double Tal2cldouble atal2;
typedef bool Tal2bool atal2;
typedef enum E0 Tal2E0 atal2;
typedef enum E1 Tal2E1 atal2;
typedef enum E2 Tal2E2 atal2;
typedef enum E3 Tal2E3 atal2;
typedef enum E4 Tal2E4 atal2;
typedef enum E5 Tal2E5 atal2;
typedef enum E6 Tal2E6 atal2;
typedef enum E7 Tal2E7 atal2;
typedef enum E8 Tal2E8 atal2;
typedef enum E9 Tal2E9 atal2;
typedef void *Tal2ptr atal2;
typedef char *Tal2cptr atal2;
typedef int *Tal2iptr atal2;
typedef char Tal4char atal4;
typedef signed char Tal4schar atal4;
typedef unsigned char Tal4uchar atal4;
typedef short int Tal4short atal4;
typedef unsigned short int Tal4ushort atal4;
typedef int Tal4int atal4;
typedef unsigned int Tal4uint atal4;
typedef long int Tal4long atal4;
typedef unsigned long int Tal4ulong atal4;
typedef long long int Tal4llong atal4;
typedef unsigned long long int Tal4ullong atal4;
typedef float Tal4float atal4;
typedef double Tal4double atal4;
typedef long double Tal4ldouble atal4;
typedef _Complex float Tal4cfloat atal4;
typedef _Complex double Tal4cdouble atal4;
typedef _Complex long double Tal4cldouble atal4;
typedef bool Tal4bool atal4;
typedef enum E0 Tal4E0 atal4;
typedef enum E1 Tal4E1 atal4;
typedef enum E2 Tal4E2 atal4;
typedef enum E3 Tal4E3 atal4;
typedef enum E4 Tal4E4 atal4;
typedef enum E5 Tal4E5 atal4;
typedef enum E6 Tal4E6 atal4;
typedef enum E7 Tal4E7 atal4;
typedef enum E8 Tal4E8 atal4;
typedef enum E9 Tal4E9 atal4;
typedef void *Tal4ptr atal4;
typedef char *Tal4cptr atal4;
typedef int *Tal4iptr atal4;
typedef char Tal8char atal8;
typedef signed char Tal8schar atal8;
typedef unsigned char Tal8uchar atal8;
typedef short int Tal8short atal8;
typedef unsigned short int Tal8ushort atal8;
typedef int Tal8int atal8;
typedef unsigned int Tal8uint atal8;
typedef long int Tal8long atal8;
typedef unsigned long int Tal8ulong atal8;
typedef long long int Tal8llong atal8;
typedef unsigned long long int Tal8ullong atal8;
typedef float Tal8float atal8;
typedef double Tal8double atal8;
typedef long double Tal8ldouble atal8;
typedef _Complex float Tal8cfloat atal8;
typedef _Complex double Tal8cdouble atal8;
typedef _Complex long double Tal8cldouble atal8;
typedef bool Tal8bool atal8;
typedef enum E0 Tal8E0 atal8;
typedef enum E1 Tal8E1 atal8;
typedef enum E2 Tal8E2 atal8;
typedef enum E3 Tal8E3 atal8;
typedef enum E4 Tal8E4 atal8;
typedef enum E5 Tal8E5 atal8;
typedef enum E6 Tal8E6 atal8;
typedef enum E7 Tal8E7 atal8;
typedef enum E8 Tal8E8 atal8;
typedef enum E9 Tal8E9 atal8;
typedef void *Tal8ptr atal8;
typedef char *Tal8cptr atal8;
typedef int *Tal8iptr atal8;
typedef char Tal16char atal16;
typedef signed char Tal16schar atal16;
typedef unsigned char Tal16uchar atal16;
typedef short int Tal16short atal16;
typedef unsigned short int Tal16ushort atal16;
typedef int Tal16int atal16;
typedef unsigned int Tal16uint atal16;
typedef long int Tal16long atal16;
typedef unsigned long int Tal16ulong atal16;
typedef long long int Tal16llong atal16;
typedef unsigned long long int Tal16ullong atal16;
typedef float Tal16float atal16;
typedef double Tal16double atal16;
typedef long double Tal16ldouble atal16;
typedef _Complex float Tal16cfloat atal16;
typedef _Complex double Tal16cdouble atal16;
typedef _Complex long double Tal16cldouble atal16;
typedef bool Tal16bool atal16;
typedef enum E0 Tal16E0 atal16;
typedef enum E1 Tal16E1 atal16;
typedef enum E2 Tal16E2 atal16;
typedef enum E3 Tal16E3 atal16;
typedef enum E4 Tal16E4 atal16;
typedef enum E5 Tal16E5 atal16;
typedef enum E6 Tal16E6 atal16;
typedef enum E7 Tal16E7 atal16;
typedef enum E8 Tal16E8 atal16;
typedef enum E9 Tal16E9 atal16;
typedef void *Tal16ptr atal16;
typedef char *Tal16cptr atal16;
typedef int *Tal16iptr atal16;
typedef char Talx1char atalx1;
typedef signed char Talx1schar atalx1;
typedef unsigned char Talx1uchar atalx1;
typedef short int Talx1short atalx1;
typedef unsigned short int Talx1ushort atalx1;
typedef int Talx1int atalx1;
typedef unsigned int Talx1uint atalx1;
typedef long int Talx1long atalx1;
typedef unsigned long int Talx1ulong atalx1;
typedef long long int Talx1llong atalx1;
typedef unsigned long long int Talx1ullong atalx1;
typedef float Talx1float atalx1;
typedef double Talx1double atalx1;
typedef long double Talx1ldouble atalx1;
typedef _Complex float Talx1cfloat atalx1;
typedef _Complex double Talx1cdouble atalx1;
typedef _Complex long double Talx1cldouble atalx1;
typedef bool Talx1bool atalx1;
typedef enum E0 Talx1E0 atalx1;
typedef enum E1 Talx1E1 atalx1;
typedef enum E2 Talx1E2 atalx1;
typedef enum E3 Talx1E3 atalx1;
typedef enum E4 Talx1E4 atalx1;
typedef enum E5 Talx1E5 atalx1;
typedef enum E6 Talx1E6 atalx1;
typedef enum E7 Talx1E7 atalx1;
typedef enum E8 Talx1E8 atalx1;
typedef enum E9 Talx1E9 atalx1;
typedef void *Talx1ptr atalx1;
typedef char *Talx1cptr atalx1;
typedef int *Talx1iptr atalx1;
typedef short int Talx2short atalx2;
typedef unsigned short int Talx2ushort atalx2;
typedef int Talx2int atalx2;
typedef unsigned int Talx2uint atalx2;
typedef long int Talx2long atalx2;
typedef unsigned long int Talx2ulong atalx2;
typedef long long int Talx2llong atalx2;
typedef unsigned long long int Talx2ullong atalx2;
typedef float Talx2float atalx2;
typedef double Talx2double atalx2;
typedef long double Talx2ldouble atalx2;
typedef _Complex float Talx2cfloat atalx2;
typedef _Complex double Talx2cdouble atalx2;
typedef _Complex long double Talx2cldouble atalx2;
typedef enum E0 Talx2E0 atalx2;
typedef enum E1 Talx2E1 atalx2;
typedef enum E2 Talx2E2 atalx2;
typedef enum E3 Talx2E3 atalx2;
typedef enum E4 Talx2E4 atalx2;
typedef enum E5 Talx2E5 atalx2;
typedef enum E6 Talx2E6 atalx2;
typedef enum E7 Talx2E7 atalx2;
typedef enum E8 Talx2E8 atalx2;
typedef enum E9 Talx2E9 atalx2;
typedef void *Talx2ptr atalx2;
typedef char *Talx2cptr atalx2;
typedef int *Talx2iptr atalx2;
typedef int Talx4int atalx4;
typedef unsigned int Talx4uint atalx4;
typedef long int Talx4long atalx4;
typedef unsigned long int Talx4ulong atalx4;
typedef long long int Talx4llong atalx4;
typedef unsigned long long int Talx4ullong atalx4;
typedef float Talx4float atalx4;
typedef double Talx4double atalx4;
typedef long double Talx4ldouble atalx4;
typedef _Complex float Talx4cfloat atalx4;
typedef _Complex double Talx4cdouble atalx4;
typedef _Complex long double Talx4cldouble atalx4;
typedef enum E0 Talx4E0 atalx4;
typedef enum E1 Talx4E1 atalx4;
typedef enum E2 Talx4E2 atalx4;
typedef enum E3 Talx4E3 atalx4;
typedef enum E4 Talx4E4 atalx4;
typedef enum E5 Talx4E5 atalx4;
typedef enum E6 Talx4E6 atalx4;
typedef enum E7 Talx4E7 atalx4;
typedef enum E8 Talx4E8 atalx4;
typedef enum E9 Talx4E9 atalx4;
typedef void *Talx4ptr atalx4;
typedef char *Talx4cptr atalx4;
typedef int *Talx4iptr atalx4;
typedef long int Taly8long ataly8;
typedef unsigned long int Taly8ulong ataly8;
typedef long long int Talx8llong atalx8;
typedef unsigned long long int Talx8ullong atalx8;
typedef double Talx8double atalx8;
typedef long double Talx8ldouble atalx8;
typedef _Complex float Talx8cfloat atalx8;
typedef _Complex double Talx8cdouble atalx8;
typedef _Complex long double Talx8cldouble atalx8;
typedef void *Taly8ptr ataly8;
typedef char *Taly8cptr ataly8;
typedef int *Taly8iptr ataly8;
typedef _Complex double Talx16cdouble atalx16;
typedef _Complex long double Talx16cldouble atalx16;
typedef int (*Tfnptr) (void);

#define T(n, fields, ops) TX(n, struct, , fields, ({ ops });)
#define U(n, fields, ops) TX(n, union, , fields, ({ ops });)
