/* These should come from genemit */

/* Use __signed__ in case compiling with -traditional.  */

typedef __signed__ char QItype;
typedef unsigned char QUtype;
typedef __signed__ short int HItype;
typedef unsigned short int HUtype;
typedef __signed__ long int SItype;
typedef unsigned long int SUtype;
typedef __signed__ long long int DItype;
typedef unsigned long long int DUtype;
typedef float SFtype;
typedef double DFtype;
typedef long double XFtype;
typedef char *Ptype;
typedef int Ttype;


typedef union stacktype
{
  QItype QIval;
  QUtype QUval;
  HItype HIval;
  HUtype HUval;
  SItype SIval;
  SUtype SUval;
  DItype DIval;
  DUtype DUval;
  SFtype SFval;
  DFtype DFval;
  XFtype XFval;
  Ptype Pval;
  Ttype Tval;
} stacktype;
