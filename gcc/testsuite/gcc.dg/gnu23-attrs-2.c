/* Test C23 attribute syntax.  Test GNU attributes appertain to
   appropriate constructs.  Attributes on types not being defined at
   the time.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -Wformat" } */

typedef void va_type (const char *, ...);
typedef va_type [[gnu::format (printf, 1, 2)]] printf_like_1;
typedef void printf_like_2 (const char *, ...) [[gnu::format (printf, 1, 2)]];
typedef __typeof__ (void (const char *, ...) [[gnu::format (printf, 1, 2)]])
  printf_like_3;

va_type func1;
printf_like_1 func2;
printf_like_2 func3;
printf_like_3 func4;
va_type [[gnu::format (printf, 1, 2)]] *func5 (void);

void
func_test (void)
{
  func1 ("%s", 1);
  func2 ("%s", 1); /* { dg-warning "expects argument" } */
  func3 ("%s", 1); /* { dg-warning "expects argument" } */
  func4 ("%s", 1); /* { dg-warning "expects argument" } */
  func5 () ("%s", 1); /* { dg-warning "expects argument" } */
}

typedef int A[2];

__typeof__ (int [[gnu::deprecated]]) var1; /* { dg-warning "deprecated" } */
__typeof__ (A [[gnu::deprecated]]) var2; /* { dg-warning "deprecated" } */
__typeof__ (int [3] [[gnu::deprecated]]) var3; /* { dg-warning "deprecated" } */
