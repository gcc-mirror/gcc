/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../../../objc-obj-c++-shared/Object1.m" } */

#include "../../../objc-obj-c++-shared/Object1.h"
extern void _exit(int);

@interface tsObj: Object {
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
    _exit (-(__LINE__)) ;
  
  return 0;
}
