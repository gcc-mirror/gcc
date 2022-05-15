/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fno-inline" } */

int foo();

#define large_code \
do { \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
  foo (); \
} while (1)


struct A
{
  char  f1;
  short f2 : 5;
  int   f3;
};

int callee1 (struct A a)
{
  if ((a.f2 + 7) & 17)
    foo ();

  if ((1300 / (short)a.f3) == 19)
    large_code;

  return 1;
}

int callee2 (short *p)
{
  if ((*p ^ 1)  <  8)
    large_code;      

  return 2;
}

int callee3 (int v)
{
  if ((27 % ((1 - (v & 0xff)) * 3)) < 6)
    {
      large_code;
      return v + 2;
    }
  else
    return v + 1;
}

int caller ()
{
  struct A a;
  short b;

  a.f2 = -7;
  a.f3 = 68;
  if (callee1 (a))
    foo ();

  a.f2 = 3;
  a.f3 = 10;
  if (callee1 (a))
    foo ();

  b = 9;
  if (callee2 (&b))
    foo ();

  b = 2;
  if (callee2 (&b))
    foo ();

  return callee3 (-5) +
	 callee3 (0); 
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node of callee1" 1 "cp" } } */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of callee2" 1 "cp" } } */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of callee3" 1 "cp" } } */
/* { dg-final { scan-ipa-dump "op0\\\[offset: 16],\\(\\(short int\\) #\\),\\(\\(int\\) #\\),\\(1300 / #\\) == 19" "cp" { target default_packed } } } */
/* { dg-final { scan-ipa-dump "op0\\\[offset: 32],\\(\\(short int\\) #\\),\\(\\(int\\) #\\),\\(1300 / #\\) == 19" "cp" { target { ! default_packed } } } } */
/* { dg-final { scan-ipa-dump "op0\\\[ref offset: 0],\\(# \\^ 1\\) <" "cp" } } */
/* { dg-final { scan-ipa-dump "op0,\\(# & 255\\),\\(1 - #\\),\\(# \\* 3\\),\\(27 % #\\) <" "cp" } } */
