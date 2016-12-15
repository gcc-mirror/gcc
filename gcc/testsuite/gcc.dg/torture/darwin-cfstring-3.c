/* Test whether the __builtin__CFStringMakeConstantString 
   "function" generates compile-time objects with the correct layout. */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-mconstant-cfstrings" } */

typedef const struct __CFString *CFStringRef;

#ifdef __CONSTANT_CFSTRINGS__
#define CFSTR(STR)  ((CFStringRef) __builtin___CFStringMakeConstantString (STR))
#else
#error __CONSTANT_CFSTRINGS__ not defined
#endif

extern int cond;
extern const char *func(void);

CFStringRef s0 = CFSTR("Hello" "there");

void foo(void) {
  const CFStringRef s1 = CFSTR("Str1");

  s0 = s1;
}

/* { dg-final { scan-assembler "\\.long\[ \\t\]+___CFConstantStringClassReference\n\[ \\t\]*\\.long\[ \\t\]+1992\n\[ \\t\]*\\.long\[ \\t\]+\[lL\]C.*\n\[ \\t\]*\\.long\[ \\t\]+4\n" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler "\\.long\[ \\t\]+___CFConstantStringClassReference\n\[ \\t\]*\\.long\[ \\t\]+1992\n\[ \\t\]*\\.long\[ \\t\]+\[lL\]C.*\n\[ \\t\]*\\.long\[ \\t\]+10\n" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler ".quad\t___CFConstantStringClassReference\n\t.long\t1992\n\t.space 4\n\t.quad\t.*\n\t.quad\t4\n" { target { *-*-darwin* && {  lp64 } } } } } */
/* { dg-final { scan-assembler ".quad\t___CFConstantStringClassReference\n\t.long\t1992\n\t.space 4\n\t.quad\t.*\n\t.quad\t10\n" { target { *-*-darwin* && {  lp64 } } } } } */
