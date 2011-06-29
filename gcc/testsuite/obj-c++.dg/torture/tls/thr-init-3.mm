/* { dg-do run } */
/* { dg-require-effective-target tls } */
/* { dg-add-options tls } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../../../objc-obj-c++-shared/TestsuiteObject.m"
extern "C" {
extern void abort ();
}

@interface tsObj: TestsuiteObject {
  int ai ;
}

- (int) fa:(int) n;

@end

@implementation tsObj

- (int) fa:(int) n
{
static __thread int as = 3;
  as += n ;
  return as ;
}

@end

int main (int ac, char *av[])
{
  int a ;
  tsObj *to = [tsObj new];
  
  a = [to fa:5];
  if ( a != 8 ) 
    abort () ;
  
  return 0;
}
