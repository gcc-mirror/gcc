/* Test the -Wnonportable-cfstrings option, which should give
   warnings if non-ASCII characters are embedded in constant
   CFStrings.  This will only work on MacOS X 10.2 and later.  */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */

/* So far, CFString is darwin-only.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "NeXT only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-mconstant-cfstrings -Wnonportable-cfstrings" } */

#import <Foundation/NSString.h>
#import <CoreFoundation/CFString.h>

#ifndef __CONSTANT_CFSTRINGS__
#error The -fconstant-cfstrings option is not functioning properly
#endif

void foo(void) {
  NSString *s1 = @"Compile-time string literal";
  CFStringRef s2 = CFSTR("Compile-time string literal");
  NSString *s3 = @"Non-ASCII literal - \222";         /* { dg-warning "non-ASCII character in CFString literal" } */
  CFStringRef s4 = CFSTR("\222 - Non-ASCII literal"); /* { dg-warning "non-ASCII character in CFString literal" } */
  CFStringRef s5 = CFSTR("Non-ASCII (\222) literal"); /* { dg-warning "non-ASCII character in CFString literal" } */
  NSString *s6 = @"\0Embedded NUL";         /* { dg-warning "embedded NUL in CFString literal" } */
  CFStringRef s7 = CFSTR("Embedded \0NUL"); /* { dg-warning "embedded NUL in CFString literal" } */
  CFStringRef s8 = CFSTR("Embedded NUL\0"); /* { dg-warning "embedded NUL in CFString literal" } */
}
