/* Test if instance methods of root classes are used as class methods, if no
   "real" methods are found.  For receivers of type 'id' and 'Class', all
   root classes must be considered.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */

#include <objc/objc.h>

#ifdef __NEXT_RUNTIME__
#define OBJC_GETCLASS objc_getClass
#else
#define OBJC_GETCLASS objc_get_class
#endif

extern void abort(void);
extern int strcmp(const char *, const char *);
#define CHECK_IF(expr) if(!(expr)) abort()

@protocol Proto
- (const char *) method4;
@end

@interface Root
{ Class isa; }
+ (const char *) method2;
@end

@interface Derived: Root
- (const char *) method1;
- (const char *) method2;
- (const char *) method3;
@end

@interface Root (Categ)
- (const char *) method3;
@end

@implementation Root (Categ)
- (const char *) method3 { return "Root(Categ)::-method3"; }
- (const char *) method4 { return "Root(Categ)::-method4"; }
@end

@implementation Derived
- (const char *) method1 { return "Derived::-method1"; }
- (const char *) method2 { return "Derived::-method2"; }
- (const char *) method3 { return "Derived::-method3"; }
@end

@implementation Root
#ifdef __NEXT_RUNTIME__
+ initialize { return self; }
#endif
- (const char *) method1 { return "Root::-method1"; }
+ (const char *) method2 { return "Root::+method2"; }
@end

int main(void)
{
  Class obj = OBJC_GETCLASS("Derived");

  /* None of the following should elicit compiler-time warnings.  */

  CHECK_IF(!strcmp([Root method1], "Root::-method1"));
  CHECK_IF(!strcmp([Root method2], "Root::+method2"));
  CHECK_IF(!strcmp([Root method3], "Root(Categ)::-method3"));
  CHECK_IF(!strcmp([Root method4], "Root(Categ)::-method4"));
  CHECK_IF(!strcmp([Derived method1], "Root::-method1"));
  CHECK_IF(!strcmp([Derived method2], "Root::+method2"));
  CHECK_IF(!strcmp([Derived method3], "Root(Categ)::-method3"));
  CHECK_IF(!strcmp([Derived method4], "Root(Categ)::-method4"));
  CHECK_IF(!strcmp([obj method1], "Root::-method1"));
  CHECK_IF(!strcmp([obj method2], "Root::+method2"));
  CHECK_IF(!strcmp([obj method3], "Root(Categ)::-method3"));
  CHECK_IF(!strcmp([obj method4], "Root(Categ)::-method4"));

  return 0;
}
