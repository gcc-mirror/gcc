/* Check CFString format extensions.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-Wall" } */

extern int printf (const char *fmt, ...);

typedef const struct __CFString * CFStringRef;

#ifdef __CONSTANT_CFSTRINGS__
#define CFSTR(cStr)  ((CFStringRef) __builtin___CFStringMakeConstantString ("" cStr ""))
#else
#error requires CFString
#endif

int s1 (CFStringRef fmt, ...) __attribute__((format(CFString, 1, 2))) ; /* OK */
int s2 (int a, CFStringRef fmt, ... ) __attribute__((format(__CFString__, 2, 3))) ; /* OK */

int s2a (int a, CFStringRef fmt, ... ) __attribute__((format(CFString, 2, 2))) ; /* { dg-error "format string argument follows the args to be formatted" } */

int s3 (const char *fmt, ... ) __attribute__((format(__CFString__, 1, 2))) ; /* { dg-error "format argument should be a .CFString. reference but a string was found" } */
int s4 (CFStringRef fmt, ... ) __attribute__((format(printf, 1, 2))) ; /* { dg-error "found a .CFStringRef.* but the format argument should be a string" } */

char *s5 (char dum, char *fmt1, ... ) __attribute__((format_arg(2))) ; /* OK */
CFStringRef s6 (CFStringRef dum, CFStringRef fmt1, ... ) __attribute__((format_arg(2))) ; /* OK */

char *s7 (int dum, void *fmt1, ... ) __attribute__((format_arg(2))) ; /* { dg-error "format string argument is not a string type" } */
int s8 (CFStringRef dum, CFStringRef fmt1, ... ) __attribute__((format_arg(2))) ; /* { dg-error "function does not return string type" } */

void foo (void)
{
  CFStringRef notchk = CFSTR ("here is an unchecked %d %s string");
  s1 (notchk, 5, 6, 7);
  printf("this one is checked %d %s", 3, 4, 5); /* { dg-warning "format .%s. expects argument of type .char .., but argument 3 has type .int." } */
			/* { dg-warning "too many arguments for format" "" { target *-*-* } .-1 } */
  printf(s5 (1, "and so is this %d %d %s", 3, 4, "hey", 6), 5, 6, 12);/* { dg-warning "format .%s. expects argument of type .char .., but argument 4 has type .int." } */
}
