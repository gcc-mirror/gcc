typedef char CHAR;
typedef short SHORT;
typedef int INT;
typedef long LONG;
typedef float FLOAT;
typedef unsigned char UCHAR;
typedef unsigned short USHORT;
typedef unsigned int UINT;
typedef unsigned long ULONG;
typedef double DOUBLE;
#if __STDC__
typedef signed char SCHAR;
typedef long double LDOUBLE;
#endif

int 
main ()
{
  typedef union
    {
      CHAR c;
      SHORT s;
      INT i;
      UCHAR uc;
      USHORT us;
      UINT ui;
      LONG l;
      ULONG ul;
      FLOAT f;
      DOUBLE d;
#if __STDC__
      SCHAR sc;
      LDOUBLE ld;
#endif
    }
  D;
  auto D D1;
  D1.c = 7;
  {
    auto struct
      {
	CHAR c;
	SHORT s;
	INT i;
	UCHAR uc;
	USHORT us;
	UINT ui;
	LONG l;
	ULONG ul;
	FLOAT f;
	DOUBLE d;
#if __STDC__
	SCHAR sc;
	LDOUBLE ld;
#endif
      }
    F;
    F.c = 7;
    if ((D1.c && F.c) != 1)
      abort ();
    if ((F.c && D1.c) != 1)
      abort ();
  }

  exit (0);
}
