/* Test for valid objc objects used in a for-each statement. */
/* FIXME: Run this test with the GNU runtime as well.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */

#include <objc/objc-api.h>
#include <Foundation/Foundation.h>

#if defined (__NEXT_RUNTIME__) && defined (__LP64__)
/* Fudge the class reference until we implement the compiler-side 
   const strings.  */
extern void *_NSConstantStringClassReference;
#endif

// gcc -o foo foo.m -framework Foundation

int main (int argc, char const* argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    NSArray * arr = [NSArray arrayWithObjects:@"A", @"B", @"C", nil];
    for (NSString * foo in arr) { 
      NSLog(@"foo is %@", foo);
    }
    [pool release];
    return 0;
}
