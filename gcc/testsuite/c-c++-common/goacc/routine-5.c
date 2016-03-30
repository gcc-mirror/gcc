/* { dg-do compile } */

#pragma acc routine /* { dg-error "not followed by" } */
int a;

#pragma acc routine /* { dg-error "not followed by" } */
void fn1 (void), fn1b (void);

#pragma acc routine /* { dg-error "not followed by" } */
int b, fn2 (void);

#pragma acc routine /* { dg-error "not followed by" } */
int fn3 (void), b2;

#pragma acc routine /* { dg-error "not followed by" } */
typedef struct c c;

#pragma acc routine /* { dg-error "not followed by" } */
struct d {} d;

#pragma acc routine /* { dg-error "not followed by" } */
#pragma acc routine
int fn4 (void);

int fn5a (void);

#pragma acc routine /* { dg-error "not followed by" } */
#pragma acc routine (fn5a)
int fn5 (void);

#ifdef __cplusplus

#pragma acc routine /* { dg-error "not followed by" "" { target c++ } } */
namespace f {}

namespace g {}

#pragma acc routine /* { dg-error "not followed by" "" { target c++ } } */
using namespace g;

#pragma acc routine (g) /* { dg-error "does not refer to" "" { target c++ } } */

#endif

#pragma acc routine (a) /* { dg-error "does not refer to" } */
  
#pragma acc routine (c) /* { dg-error "does not refer to" } */


void Bar ();

void Foo ()
{
  Bar ();
}

#pragma acc routine (Bar) // { dg-error "must be applied before use" }

#pragma acc routine (Foo) gang // { dg-error "must be applied before definition" }

#pragma acc routine (Baz) // { dg-error "not been declared" }


int vb1;		/* { dg-error "directive for use" } */
extern int vb2;		/* { dg-error "directive for use" } */
static int vb3;		/* { dg-error "directive for use" } */

#pragma acc routine
int
func1 (int a)
{
  vb1 = a + 1;
  vb2 = vb1 + 1;
  vb3 = vb2 + 1;

  return vb3;
}

#pragma acc routine
int
func2 (int a)
{
  extern int vb4;	/* { dg-error "directive for use" } */
  static int vb5;	/* { dg-error "directive for use" } */

  vb4 = a + 1;
  vb5 = vb4 + 1;

  return vb5;
}

extern int vb6;			/* { dg-error "clause used in" } */
#pragma acc declare link (vb6)
static int vb7;			/* { dg-error "clause used in" } */
#pragma acc declare link (vb7)

#pragma acc routine
int
func3 (int a)
{
  vb6 = a + 1;
  vb7 = vb6 + 1;

  return vb7;
}

int vb8;
#pragma acc declare create (vb8)
extern int vb9;
#pragma acc declare create (vb9)
static int vb10;
#pragma acc declare create (vb10)

#pragma acc routine
int
func4 (int a)
{
  vb8 = a + 1;
  vb9 = vb8 + 1;
  vb10 = vb9 + 1;

  return vb10;
}

int vb11;
#pragma acc declare device_resident (vb11)
extern int vb12;
#pragma acc declare device_resident (vb12)
extern int vb13;
#pragma acc declare device_resident (vb13)

#pragma acc routine
int
func5 (int a)
{
  vb11 = a + 1;
  vb12 = vb11 + 1;
  vb13 = vb12 + 1;

  return vb13;
}

#pragma acc routine
int
func6 (int a)
{
  extern int vb14;
#pragma acc declare create (vb14)
  static int vb15;
#pragma acc declare create (vb15)

  vb14 = a + 1;
  vb15 = vb14 + 1;

  return vb15;
}
