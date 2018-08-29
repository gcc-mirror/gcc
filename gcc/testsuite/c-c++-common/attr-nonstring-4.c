/* PR middle-end/83131 - c-c++/common/attr-nonstring-3 failure for strcmp
   tests on PowerPC
   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow -ftrack-macro-expansion=0" }  */

#if __cplusplus
extern "C" {
#endif

typedef __SIZE_TYPE__ size_t;

extern int strcmp (const char*, const char*);
extern int strncmp (const char*, const char*, size_t);

#if __cplusplus
}   /* extern "C" */
#endif

extern char arx[] __attribute__ ((nonstring));
extern char ar5[5] __attribute__ ((nonstring));
extern char str[];

enum { N = sizeof ar5 };
enum { X = sizeof ar5 + 1 };


int warn_strcmp_cst_1 (void)
{
  return strcmp ("bar", arx);       /* { dg-warning "argument 2 declared attribute .nonstring." } */
}

int warn_strcmp_cst_2 (void)
{
  return strcmp (arx, "foo");       /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


int warn_strncmp_cst_1 (void)
{
  return strncmp ("12345", ar5, X);   /* { dg-warning "argument 2 declared attribute .nonstring." } */
}

int warn_strncmp_cst_2 (void)
{
  return strncmp (ar5, "12345", X);   /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


int nowarn_strncmp_cst_1 (void)
{
  return strncmp ("12345", ar5, N);
}

int nowarn_strncmp_cst_2 (void)
{
  return strncmp (ar5, "12345", N);
}


int warn_strncmp_var_1 (void)
{
  return strncmp (str, ar5, X);     /* { dg-warning "argument 2 declared attribute .nonstring." } */
}

int warn_strncmp_var_2 (void)
{
  return strncmp (ar5, str, X);     /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


int nowarn_strncmp_var_1 (void)
{
  return strncmp (str, ar5, N);
}

int nowarn_strncmp_var_2 (void)
{
  return strncmp (ar5, str, N);
}
