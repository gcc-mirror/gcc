/* Test whether the __builtin__CFStringMakeConstantString 
   "function" fails gracefully when used without the
   -mconstant-cfstrings flag.  */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-mno-constant-cfstrings" } */

typedef const struct __CFString *CFStringRef;

const CFStringRef S = ((CFStringRef)__builtin___CFStringMakeConstantString("Testing"));
/* { dg-error "built-in" "built-in function .* requires .* flag" { target *-*-* } 11 } */
