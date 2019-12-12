/* Test for valid objc objects used in a for-each statement. */
/* FIXME: Run this test with the GNU runtime as well.  */
/* { dg-do run { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-framework Foundation" { target { *-*-darwin* } } } */

#include "../objc-obj-c++-shared/F-NSString.h"
#include "../objc-obj-c++-shared/F-NSAutoreleasePool.h"
#include "../objc-obj-c++-shared/F-NSArray.h"

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
