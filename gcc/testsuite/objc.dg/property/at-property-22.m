/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test properties of different types.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

enum colour { Red, Black };

@interface MyRootClass
{
  Class isa;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
+ (Class) class;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
+ (Class) class { return self; }
@end


@interface MyClass : MyRootClass
{
  /* A bunch of C types.  */
  char         pchar;
  short        pshort;
  int          pint;
  long         plong;
  float        pfloat;
  double       pdouble;
  enum colour  penum;

  /* A bunch of pointers to C types.  */
  char        *pcharp;
  short       *pshortp;
  int         *pintp;
  long        *plongp;
  float       *pfloatp;
  double      *pdoublep;
  enum colour *penump;

  /* A bunch of Objective-C types.  */
  id           pid;
  Class        pclass;
  MyClass     *pMyClassp;
}
@property (assign) char pchar;
@property (assign) short pshort;
@property (assign) int pint;
@property (assign) long plong;
@property (assign) float pfloat;
@property (assign) double pdouble;
@property (assign) enum colour penum;

@property (assign) char *pcharp;
@property (assign) short *pshortp;
@property (assign) int *pintp;
@property (assign) long *plongp;
@property (assign) float *pfloatp;
@property (assign) double *pdoublep;
@property (assign) enum colour *penump;

@property (assign) id pid;
@property (assign) Class pclass;
@property (assign) MyClass *pMyClassp;
@end

@implementation MyClass
@synthesize pchar;
@synthesize pshort;
@synthesize pint;
@synthesize plong;
@synthesize pfloat;
@synthesize pdouble;
@synthesize penum;

@synthesize pcharp;
@synthesize pshortp;
@synthesize pintp;
@synthesize plongp;
@synthesize pfloatp;
@synthesize pdoublep;
@synthesize penump;

@synthesize pid;
@synthesize pclass;
@synthesize pMyClassp;
@end

int main (void)
{
  MyClass *object = [[MyClass alloc] init];

  object.pchar = 1;
  if (object.pchar != 1)
    abort ();

  object.pshort = 2;
  if (object.pshort != 2)
    abort ();

  object.pint = 3;
  if (object.pint != 3)
    abort ();

  object.plong = 4;
  if (object.plong != 4)
    abort ();

  object.pfloat = 0;
  if (object.pfloat != 0)
    abort ();

  object.pdouble = 0;
  if (object.pdouble != 0)
    abort ();

  object.penum = Black;
  if (object.penum != Black)
    abort ();

  object.pcharp = (char *)0;
  if (object.pcharp != 0)
    abort ();
  
  object.pshortp = (short *)0;
  if (object.pshortp != 0)
    abort ();

  object.pintp = (int *)0;
  if (object.pintp != 0)
    abort ();
    
  object.plongp = (long *)0;
  if (object.plongp != 0)
    abort ();
    
  object.pfloatp = (float *)0;
  if (object.pfloatp != 0)
    abort ();
    
  object.pdoublep = (double *)0;
  if (object.pdoublep != 0)
    abort ();
    
  object.penump = (enum colour *)0;
  if (object.penump != 0)
    abort ();

  object.pid = object;
  if (object.pid != object)
    abort ();

  object.pclass = [MyClass class];
  if (object.pclass != [MyClass class])
    abort ();

  object.pMyClassp = object;
  if (object.pMyClassp != object)
    abort ();

  return 0;
}
