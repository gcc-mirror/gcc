/* PR middle-end/85359 - duplicate -Wstringop-overflow for a strcmp call
   with a nonstring pointer
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__       size_t;
typedef __builtin_va_list   va_list;

int printf (const char*, ...);
int puts (const char*);
int puts_unlocked (const char*);
int sprintf (char*, const char*, ...);
int snprintf (char*, size_t, const char*, ...);
int vsprintf (char*, const char*, va_list);
int vsnprintf (char*, size_t, const char*, va_list);

int strcmp (const char*, const char*);
int strncmp (const char*, const char*, size_t);

char* stpcpy (char*, const char*);
char* stpncpy (char*, const char*, size_t);

char* strcat (char*, const char*);
char* strncat (char*, const char*, size_t);

char* strcpy (char*, const char*);
char* strncpy (char*, const char*, size_t);

char* strchr (const char*, int);
char* strrchr (const char*, int);
char* strstr (const char*, const char*);
char* strdup (const char*);
size_t strlen (const char*);
size_t strnlen (const char*, size_t);
char* strndup (const char*, size_t);

#define NONSTRING __attribute__ ((nonstring))

extern char ns5[5] NONSTRING;

int strcmp_nonstring_1 (NONSTRING const char *a, const char *b)
{
  /* dg-warning matches one or more instances of the warning so it's
     no good on its own.  Use dg-regexp instead to verify that just
     one instance of the warning is issued.  See gcc.dg/pr64223-1
     for a different approach.  */
  return strcmp (a, b);  /* { dg-regexp "\[^\n\r\]+: warning: .strcmp. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strcmp" } */
}

int strcmp_nonstring_2 (const char *a, NONSTRING const char *b)
{
  return strcmp (a, b);  /* { dg-regexp "\[^\n\r\]+: warning: .strcmp. argument 2 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strcmp" } */
}

int strncmp_nonstring_1 (const char *s)
{
  return strncmp (s, ns5, sizeof ns5 + 1);  /* { dg-regexp "\[^\n\r\]+: warning: .strncmp. argument 2 declared attribute .nonstring. \[^\n\r\]+ \\\[-Wstringop-overread\[^\n\r\]*" "strncmp" } */
}

int strncmp_nonstring_2 (const char *s)
{
  return strncmp (ns5, s, sizeof ns5 + 1);  /* { dg-regexp "\[^\n\r\]+: warning: .strncmp. argument 1 declared attribute .nonstring. \[^\n\r\]+ \\\[-Wstringop-overread\[^\n\r\]*" "strncmp" } */
}

char* stpcpy_nonstring (char *d, NONSTRING const char *s)
{
  return stpcpy (d, s);  /* { dg-regexp "\[^\n\r\]+: warning: .stpcpy. argument 2 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "stpcpy" } */
}

char* stpncpy_nonstring (char *d)
{
  return stpncpy (d, ns5, sizeof ns5 + 1);  /* { dg-regexp "\[^\n\r\]+: warning: .stpncpy. argument 2 declared attribute .nonstring. \[^\n\r\]+ \\\[-Wstringop-overread\[^\n\r\]*" "stpncpy" } */
}

char* strchr_nonstring (NONSTRING const char *s, int c)
{
  return strchr (s, c);  /* { dg-regexp "\[^\n\r\]+: warning: .strchr. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strchr" } */
}

char* strrchr_nonstring (NONSTRING const char *s, int c)
{
  return strrchr (s, c);  /* { dg-regexp "\[^\n\r\]+: warning: .strrchr. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strrchr" } */
}

char* strcpy_nonstring (char *d, NONSTRING const char *s)
{
  return strcpy (d, s);  /* { dg-regexp "\[^\n\r\]+: warning: .strcpy. argument 2 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strcpy" } */
}

char* strncpy_nonstring (char *d)
{
  return strncpy (d, ns5, sizeof ns5 + 1);  /* { dg-regexp "\[^\n\r\]+: warning: .strncpy. argument 2 declared attribute .nonstring. \[^\n\r\]+ \\\[-Wstringop-overread\[^\n\r\]*" "strncpy" } */
}

char* strstr_nonstring_1 (NONSTRING const char *a, const char *b)
{
  return strstr (a, b);  /* { dg-regexp "\[^\n\r\]+: warning: .strstr. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strstr" } */
}

char* strstr_nonstring_2 (const char *a, NONSTRING const char *b)
{
  return strstr (a, b);  /* { dg-regexp "\[^\n\r\]+: warning: .strstr. argument 2 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strstr" } */
}

char* stdup_nonstring (NONSTRING const char *s)
{
  return strdup (s);  /* { dg-regexp "\[^\n\r\]+: warning: .strdup. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strdup" } */
}

size_t strlen_nonstring (NONSTRING const char *s)
{
  return strlen (s);  /* { dg-regexp "\[^\n\r\]+: warning: .strlen. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "strlen" } */
}

int printf_nonstring (NONSTRING const char *s)
{
  return printf (s);  /* { dg-regexp "\[^\n\r\]+: warning: .printf. argument 1 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "printf" } */
}

int sprintf_nonstring_2 (char *d, NONSTRING const char *s)
{
  return sprintf (d, s);  /* { dg-regexp "\[^\n\r\]+: warning: .sprintf. argument 2 declared attribute .nonstring. \\\[-Wstringop-overread\[^\n\r\]*" "sprintf" } */
}
