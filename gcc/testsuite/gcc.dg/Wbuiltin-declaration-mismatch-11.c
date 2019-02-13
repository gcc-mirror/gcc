/* PR c/86125 */
/* { dg-do compile } */
/* { dg-options "-Wbuiltin-declaration-mismatch -Wextra -Wno-ignored-qualifiers" } */

typedef __SIZE_TYPE__ size_t;
struct FILE;
struct tm;
struct fenv_t;
struct fexcept_t;
typedef struct FILE FILE;
typedef struct fenv_t fenv_t;
typedef struct fexcept_t fexcept_t;
typedef const int cint;
size_t strftime (char *__restrict, const size_t, const char *__restrict,	/* { dg-bogus "mismatch in argument 1 type of built-in function" } */
                 const struct tm *__restrict) __attribute__((nothrow));
int fprintf (struct FILE *, const char *const, ...);				/* { dg-bogus "mismatch in argument 2 type of built-in function" } */
cint putc (int, struct FILE *);							/* { dg-bogus "mismatch in return type of built-in function" } */
cint fegetenv (fenv_t *);							/* { dg-bogus "mismatch in argument 1 type of built-in function" } */
cint fesetenv (const fenv_t *);							/* { dg-bogus "mismatch in return type of built-in function" } */
int fegetexceptflag (fexcept_t *, const int);					/* { dg-bogus "mismatch in argument 1 type of built-in function" } */
int fesetexceptflag (const fexcept_t *, const int);				/* { dg-bogus "mismatch in argument 1 type of built-in function" } */
