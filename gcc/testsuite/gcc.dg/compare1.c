/* Test for a bogus warning on comparison between signed and unsigned.
   This was inspired by code in gcc.  This testcase is identical to
   compare9.c except that we use -fno-short-enums here and expect a
   warning from case 4.  */

/* { dg-do compile } */
/* { dg-options "-fno-short-enums -Wsign-compare" } */

int tf = 1;

/* This enumeration has an explicit negative value and is therefore signed.  */
enum mm1 
{
  VOID, SI, DI, MAX = -1
};

/* This enumeration fits entirely in a signed int, but is unsigned anyway.  */
enum mm2
{
  VOID2, SI2, DI2, MAX2
};

int f(enum mm1 x)
{
  return x == (tf?DI:SI); /* { dg-bogus "changes signedness" "case 1" } */
}

int g(enum mm1 x)
{
  return x == (tf?DI:-1); /* { dg-bogus "changes signedness" "case 2" } */
}

int h(enum mm2 x)
{
  return x == (tf?DI2:SI2); /* { dg-bogus "changes signedness" "case 3" } */
}

int i(enum mm2 x)
{
  return x == (tf?DI2:-1); /* { dg-warning "different signedness" "case 4" } */
}
