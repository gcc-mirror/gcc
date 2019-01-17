/* { dg-do compile } */
/* { dg-additional-options "-mlocal-symbol-id=" { target amdgcn-*-* } } */
/* { dg-final { scan-assembler-not {foo[1-9]\.[0-9]} } } */

/* Check that we don't get .0 suffixes on static variables when not using
   intermodule analysis.  */

static int foo1;
static int foo2 = 1;

static void foo5(void) {  }
static void foo6(void);
static void foo6(void) { }
static void foo7(void);
void foo7(void) { }

void foo9(void) 
{
  foo1 = 2;
  foo2 = 3;
  foo5();
  foo6();
  foo7();
}
