#include <stdio.h>
#include <string.h>

char out[100];

typedef struct { double d; int i[3]; } B;
typedef struct { char c[33],c1; } X;

char c1 = 'a';
char c2 = 127;
char c3 = (char)128;
char c4 = (char)255;
char c5 = -1;

double d1 = 0.1;
double d2 = 0.2;
double d3 = 0.3;
double d4 = 0.4;
double d5 = 0.5;
double d6 = 0.6;
double d7 = 0.7;
double d8 = 0.8;
double d9 = 0.9;

B B1 = {0.1,{1,2,3}};
B B2 = {0.2,{5,4,3}};
X X1 = {"abcdefghijklmnopqrstuvwxyzABCDEF", 'G'};
X X2 = {"123",'9'};
X X3 = {"return-return-return",'R'};

X f (B a, char b, double c, B d)
{
  static X xr = {"return val", 'R'};
  X r;
  r = xr;
  r.c1 = b;
  sprintf (out, "X f(B,char,double,B):({%g,{%d,%d,%d}},'%c',%g,{%g,{%d,%d,%d}})",
	   a.d, a.i[0], a.i[1], a.i[2], b, c, d.d, d.i[0], d.i[1], d.i[2]);
  return r;
}

X (*fp) (B, char, double, B) = &f;

main ()
{
  X Xr;
  char tmp[100];

  Xr = f (B1, c2, d3, B2);
  strcpy (tmp, out);
  Xr.c[0] = Xr.c1 = '\0';
  Xr = (*fp) (B1, c2, d3, B2);
  if (strcmp (tmp, out))
    abort ();

  exit (0);
}
