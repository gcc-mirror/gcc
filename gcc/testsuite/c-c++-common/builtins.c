/* Test to verify that calls to common built-in functions declared
   with no prototype do not cause an ICE.
  { dg-do compile }
  { dg-options "-O2 -Wall -Wextra" }
  { dg-additional-options "-fpermissive" { target c } }
  { dg-prune-output "warning" }
  { dg-require-effective-target alloca }  */

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
extern "C" {

#define NO_PROTO ...
#else
#define NO_PROTO /* empty */
#endif

  /* Character classification built-ins from <ctype.h>.  */
  int isalpha (NO_PROTO);
  int isalnum (NO_PROTO);
  int isalpha (NO_PROTO);
  int iscntrl (NO_PROTO);
  int isdigit (NO_PROTO);
  int isgraph (NO_PROTO);
  int islower (NO_PROTO);
  int isprint (NO_PROTO);
  int ispunct (NO_PROTO);
  int isspace (NO_PROTO);
  int isupper (NO_PROTO);
  int isxdigit (NO_PROTO);
  int tolower (NO_PROTO);
  int toupper (NO_PROTO);

  /* Memory allocation built-ins from <stdlib.h>.  */
  void* alloca (NO_PROTO);
  void* aligned_alloc (NO_PROTO);
  void* calloc (NO_PROTO);
  void* malloc (NO_PROTO);
  void* realloc (NO_PROTO);

  /* Raw memory built-ins from <string.h>.  */
  void* memcpy (NO_PROTO);
  void* memchr (NO_PROTO);
  void* memmove (NO_PROTO);
  void* mempcpy (NO_PROTO);
  void* memset (NO_PROTO);

  /* String built-ins from <string.h>.  */
  char* stpcpy (NO_PROTO);
  char* stpncpy (NO_PROTO);

  char* strcat (NO_PROTO);
  char* strcpy (NO_PROTO);

  char* strdup (NO_PROTO);
  char* strndup (NO_PROTO);

  char* strncat (NO_PROTO);
  char* strncpy (NO_PROTO);

  size_t strlen (NO_PROTO);
  size_t strnlen (NO_PROTO);

  char* strchr (NO_PROTO);
  int strcmp (NO_PROTO);
  int strncmp (NO_PROTO);

  /* Input/output functions from <stdio.h>.  */
  int puts (NO_PROTO);
  int fputs (NO_PROTO);

  int scanf (NO_PROTO);
  int fscanf (NO_PROTO);
  int sscanf (NO_PROTO);
  int vfscanf (NO_PROTO);
  int vsscanf (NO_PROTO);

  int printf (NO_PROTO);
  int fprintf (NO_PROTO);
  int sprintf (NO_PROTO);

  int snprintf (NO_PROTO);

  int vprintf (NO_PROTO);
  int vfprintf (NO_PROTO);
  int vsprintf (NO_PROTO);

  int vsnprintf (NO_PROTO);

#if __cplusplus
}
#endif


#define CONCAT(a, b) a ## b
#define UNIQ_NAME(func, id) CONCAT (test_ ## func ## _, id)

#define TEST_FUNC(func, arglist)		\
  __typeof__ (func arglist)			\
  UNIQ_NAME (func, __COUNTER__) (void) {	\
    return func arglist;			\
  }

#define T1(func)				\
  TEST_FUNC (func, ());				\
  TEST_FUNC (func, (1));			\
  TEST_FUNC (func, (""));			\
  TEST_FUNC (func, ((void*)1));			\
  TEST_FUNC (func, (iarr));			\
  TEST_FUNC (func, (function))

#define T2(func)				\
  TEST_FUNC (func, (1, 1));			\
  TEST_FUNC (func, (1, ""));			\
  TEST_FUNC (func, (1, (void*)1));		\
  TEST_FUNC (func, (1, iarr));			\
  TEST_FUNC (func, (1, function))

#define T3(func)				\
  TEST_FUNC (func, (1, 1, 1));			\
  TEST_FUNC (func, (1, 1, ""));			\
  TEST_FUNC (func, (1, 1, (void*)1));		\
  TEST_FUNC (func, (1, 1, iarr));		\
  TEST_FUNC (func, (1, 1, function))

extern int iarr[];
extern void function (void);

T1 (isalpha);
T1 (isalnum);
T1 (isalpha);
T1 (iscntrl);
T1 (isdigit);
T1 (isgraph);
T1 (islower);
T1 (isprint);
T1 (ispunct);
T1 (isspace);
T1 (isupper);
T1 (isxdigit);
T1 (tolower);
T1 (toupper);

T1 (alloca);
T2 (aligned_alloc);
T2 (malloc);
T2 (calloc);
T2 (realloc);

T3 (memcpy);
T3 (memmove);
T3 (mempcpy);
T3 (memset);
T3 (memchr);

T2 (stpcpy);
T3 (stpncpy);

T2 (strcat);
T2 (strcpy);

T1 (strdup);
T2 (strndup);

T3 (strncat);
T3 (strncpy);

T2 (strchr);
T2 (strcmp);
T3 (strncmp);

T1 (strlen);
T2 (strnlen);

T1 (puts);
T2 (fputs);

T1 (scanf);
T2 (fscanf);
T2 (sscanf);
T2 (vfscanf);
T2 (vsscanf);

T2 (printf);
T3 (fprintf);
T3 (sprintf);

T3 (snprintf);

T2 (vprintf);
T2 (vfprintf);
T2 (vsprintf);

T3 (vsnprintf);
