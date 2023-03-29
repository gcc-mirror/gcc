/* For the S/390 ABI parameter D is passed in GPR 5 and 6 and the latter is
   call-saved which prohibits sibling call optimization.  This must hold true
   also if the mode of the parameter is BLKmode.  */

typedef struct
{
  short x;
  char y[6];
} t;

extern t d;

extern void bar (int a, int b, int c, t d);

void foo (int a, int b, int c)
{
  bar (a, b, c, d);
}
