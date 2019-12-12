/* PR c/68833 */
/* { dg-do compile } */
/* { dg-options "-Werror=larger-than-65536 -Werror=format -Werror=missing-noreturn" } */
/* { dg-require-effective-target int32plus } */

int a[131072];	/* { dg-error "size of .a. 524288 bytes exceeds maximum object size 65536" } */
int b[1024];	/* { dg-bogus "size" } */

void
f1 (const char *fmt)
{
  __builtin_printf ("%d\n", 1.2);	/* { dg-error "expects argument of type" } */
  __builtin_printf (fmt, 1.2);		/* { dg-bogus "format not a string literal, argument types not checked" } */
}

extern void f2 (void);
void
f2 (void) /* { dg-error "candidate for attribute 'noreturn'" "detect noreturn candidate" } */
{
  __builtin_exit (0);
}

/* { dg-prune-output "treated as errors" } */
