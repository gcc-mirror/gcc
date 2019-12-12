/* PR tree-optimization/91567 - Spurious -Wformat-overflow warnings building
   glibc (32-bit only)
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern int sprintf (char*, const char*, ...);
extern size_t strlen (const char*);

void f (char *);

void g (char *s1, char *s2)
{
  char b[1025];
  size_t n = __builtin_strlen (s1), d = __builtin_strlen (s2);
  if (n + d + 1 >= 1025)
    return;

  sprintf (b, "%s.%s", s1, s2);     // { dg-bogus "\\\[-Wformat-overflow" }

  f (b);
}

/* Extracted from gcc/c-cppbuiltin.c.  */

void cpp_define (char*);

static void
builtin_define_type_minmax (const char *min_macro, const char *max_macro,
			    void *type)
{
  extern const char *suffix;
  char *buf;

  if (type)
    {
      buf = (char *) __builtin_alloca (__builtin_strlen (min_macro) + 2
				       + __builtin_strlen (suffix) + 1);
      sprintf (buf, "%s=0%s", min_macro, suffix);      // { dg-bogus "\\\[-Wformat-overflow" }
    }
  else
    {
      buf = (char *) __builtin_alloca (__builtin_strlen (min_macro) + 3
				       + __builtin_strlen (max_macro) + 6);
      sprintf (buf, "%s=(-%s - 1)", min_macro, max_macro);  // { dg-bogus "\\\[-Wformat-overflow" }
    }

  cpp_define (buf);
}

void
c_cpp_builtins (void *type)
{

  builtin_define_type_minmax ("__WCHAR_MIN__", "__WCHAR_MAX__", type);
  builtin_define_type_minmax ("__WINT_MIN__", "__WINT_MAX__", type);
}
