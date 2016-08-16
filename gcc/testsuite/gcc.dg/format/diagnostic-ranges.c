/* { dg-options "-Wformat -fdiagnostics-show-caret" } */

/* See PR 52952. */

#include "format.h"

void test_mismatching_types (const char *msg)
{
  printf("hello %i", msg);  /* { dg-warning "format '%i' expects argument of type 'int', but argument 2 has type 'const char \\*' " } */

/* TODO: ideally would also underline "msg".  */
/* { dg-begin-multiline-output "" }
   printf("hello %i", msg);
                 ~^
                 %s
   { dg-end-multiline-output "" } */


  printf("hello %s", 42);  /* { dg-warning "format '%s' expects argument of type 'char \\*', but argument 2 has type 'int'" } */
/* TODO: ideally would also underline "42".  */
/* { dg-begin-multiline-output "" }
   printf("hello %s", 42);
                 ~^
                 %d
   { dg-end-multiline-output "" } */


  printf("hello %i", (long)0);  /* { dg-warning "format '%i' expects argument of type 'int', but argument 2 has type 'long int' " } */
/* TODO: ideally would also underline the argument.  */
/* { dg-begin-multiline-output "" }
   printf("hello %i", (long)0);
                 ~^
                 %ld
   { dg-end-multiline-output "" } */
}

void test_multiple_arguments (void)
{
  printf ("arg0: %i  arg1: %s arg 2: %i", /* { dg-warning "29: format '%s'" } */
          100, 101, 102);
/* TODO: ideally would also underline "101".  */
/* { dg-begin-multiline-output "" }
   printf ("arg0: %i  arg1: %s arg 2: %i",
                            ~^
                            %d
   { dg-end-multiline-output "" } */
}

void test_multiple_arguments_2 (int i, int j)
{
  printf ("arg0: %i  arg1: %s arg 2: %i", /* { dg-warning "29: format '%s'" } */
          100, i + j, 102);
/* { dg-begin-multiline-output "" }
   printf ("arg0: %i  arg1: %s arg 2: %i",
                            ~^
                            %d
           100, i + j, 102);
                ~~~~~         
   { dg-end-multiline-output "" } */
}

void multiline_format_string (void) {
  printf ("before the fmt specifier" /* { dg-warning "11: format '%d' expects a matching 'int' argument" } */
/* { dg-begin-multiline-output "" }
   printf ("before the fmt specifier"
           ^~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

          "%"
          "d" /* { dg-message "12: format string is defined here" } */
          "after the fmt specifier");

/* { dg-begin-multiline-output "" }
           "%"
            ~~
           "d"
           ~^
   { dg-end-multiline-output "" } */
}

void test_hex (const char *msg)
{
  /* "%" is \x25
     "i" is \x69 */
  printf("hello \x25\x69", msg);  /* { dg-warning "format '%i' expects argument of type 'int', but argument 2 has type 'const char \\*' " } */

/* TODO: ideally would also underline "msg".  */
/* { dg-begin-multiline-output "" }
   printf("hello \x25\x69", msg);
                 ~~~~^~~~
                 %s
   { dg-end-multiline-output "" } */
}

void test_oct (const char *msg)
{
  /* "%" is octal 045
     "i" is octal 151.  */
  printf("hello \045\151", msg);  /* { dg-warning "format '%i' expects argument of type 'int', but argument 2 has type 'const char \\*' " } */

/* TODO: ideally would also underline "msg".  */
/* { dg-begin-multiline-output "" }
   printf("hello \045\151", msg);
                 ~~~~^~~~
                 %s
   { dg-end-multiline-output "" } */
}

void test_multiple (const char *msg)
{
  /* "%" is \x25 in hex
     "i" is \151 in octal.  */
  printf("prefix"  "\x25"  "\151"  "suffix",  /* { dg-warning "format '%i'" } */
         msg);
/* { dg-begin-multiline-output "" }
   printf("prefix"  "\x25"  "\151"  "suffix",
          ^~~~~~~~
  { dg-end-multiline-output "" } */

/* TODO: ideally would also underline "msg".  */
/* { dg-begin-multiline-output "" }
   printf("prefix"  "\x25"  "\151"  "suffix",
                     ~~~~~~~~^~~~
                     %s
  { dg-end-multiline-output "" } */
}

void test_u8 (const char *msg)
{
  printf(u8"hello %i", msg);/* { dg-warning "format '%i' expects argument of type 'int', but argument 2 has type 'const char \\*' " } */
/* TODO: ideally would also underline "msg".  */
/* { dg-begin-multiline-output "" }
   printf(u8"hello %i", msg);
                   ~^
                   %s
   { dg-end-multiline-output "" } */
}

void test_param (long long_i, long long_j)
{
  printf ("foo %s bar", long_i + long_j); /* { dg-warning "17: format '%s' expects argument of type 'char \\*', but argument 2 has type 'long int'" } */
/* { dg-begin-multiline-output "" }
   printf ("foo %s bar", long_i + long_j);
                ~^       ~~~~~~~~~~~~~~~
                %ld
   { dg-end-multiline-output "" } */
}

void test_field_width_specifier (long l, int i1, int i2)
{
  printf (" %*.*d ", l, i1, i2); /* { dg-warning "14: field width specifier '\\*' expects argument of type 'int', but argument 2 has type 'long int'" } */
/* { dg-begin-multiline-output "" }
   printf (" %*.*d ", l, i1, i2);
             ~^~~~
   { dg-end-multiline-output "" } */
}

/* PR c/72857.  */

void test_field_width_specifier_2 (char *d, long foo, long bar)
{
  __builtin_sprintf (d, " %*ld ", foo, foo); /* { dg-warning "28: field width specifier '\\*' expects argument of type 'int', but argument 3 has type 'long int'" } */
  /* TODO: ideally we'd underline the first "foo" here".  */
  /* { dg-begin-multiline-output "" }
   __builtin_sprintf (d, " %*ld ", foo, foo);
                           ~^~~
   { dg-end-multiline-output "" } */

  __builtin_sprintf (d, " %*ld ", foo + bar, foo); /* { dg-warning "28: field width specifier '\\*' expects argument of type 'int', but argument 3 has type 'long int'" } */
  /* { dg-begin-multiline-output "" }
   __builtin_sprintf (d, " %*ld ", foo + bar, foo);
                           ~^~~    ~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_field_precision_specifier (char *d, long foo, long bar)
{
  __builtin_sprintf (d, " %.*ld ", foo, foo); /* { dg-warning "29: field precision specifier '\\.\\*' expects argument of type 'int', but argument 3 has type 'long int'" } */
  /* TODO: ideally we'd underline the first "foo" here".  */
  /* { dg-begin-multiline-output "" }
   __builtin_sprintf (d, " %.*ld ", foo, foo);
                           ~~^~~
   { dg-end-multiline-output "" } */

  __builtin_sprintf (d, " %.*ld ", foo + bar, foo); /* { dg-warning "29: field precision specifier '\\.\\*' expects argument of type 'int', but argument 3 has type 'long int'" } */
  /* { dg-begin-multiline-output "" }
   __builtin_sprintf (d, " %.*ld ", foo + bar, foo);
                           ~~^~~    ~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_spurious_percent (void)
{
  printf("hello world %"); /* { dg-warning "23: spurious trailing" } */

/* { dg-begin-multiline-output "" }
   printf("hello world %");
                       ^
   { dg-end-multiline-output "" } */
}

void test_empty_precision (char *s, size_t m, double d)
{
  strfmon (s, m, "%#.5n", d); /* { dg-warning "20: empty left precision in gnu_strfmon format" } */
/* { dg-begin-multiline-output "" }
   strfmon (s, m, "%#.5n", d);
                    ^
   { dg-end-multiline-output "" } */

  strfmon (s, m, "%#5.n", d); /* { dg-warning "22: empty precision in gnu_strfmon format" } */
/* { dg-begin-multiline-output "" }
   strfmon (s, m, "%#5.n", d);
                      ^
   { dg-end-multiline-output "" } */
}

void test_repeated (int i)
{
  printf ("%++d", i); /* { dg-warning "14: repeated '\\+' flag in format" } */
/* { dg-begin-multiline-output "" }
   printf ("%++d", i);
              ^
   { dg-end-multiline-output "" } */
}

void test_conversion_lacks_type (void)
{
  printf (" %h"); /* { dg-warning "14:conversion lacks type at end of format" } */
/* { dg-begin-multiline-output "" }
   printf (" %h");
              ^
   { dg-end-multiline-output "" } */
}

void test_embedded_nul (void)
{
  printf (" \0 "); /* { dg-warning "13:embedded" "warning for embedded NUL" } */
/* { dg-begin-multiline-output "" }
   printf (" \0 ");
             ^~
   { dg-end-multiline-output "" } */
}

void test_macro (const char *msg)
{
#define INT_FMT "%i" /* { dg-message "19: format string is defined here" } */
  printf("hello " INT_FMT " world", msg);  /* { dg-warning "10: format '%i' expects argument of type 'int', but argument 2 has type 'const char \\*' " } */
/* { dg-begin-multiline-output "" }
   printf("hello " INT_FMT " world", msg);
          ^~~~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
 #define INT_FMT "%i"
                  ~^
                  %s
   { dg-end-multiline-output "" } */
}

void test_non_contiguous_strings (void)
{
  __builtin_printf(" %" "d ", 0.5); /* { dg-warning "20: format .%d. expects argument of type .int., but argument 2 has type .double." } */
                                    /* { dg-message "26: format string is defined here" "" { target *-*-* } 261 } */
  /* { dg-begin-multiline-output "" }
   __builtin_printf(" %" "d ", 0.5);
                    ^~~~
   { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   __builtin_printf(" %" "d ", 0.5);
                      ~~~~^
                      %f
   { dg-end-multiline-output "" } */
}

void test_const_arrays (void)
{
  /* TODO: ideally we'd highlight both the format string *and* the use of
     it here.  For now, just verify that we gracefully handle this case.  */
  const char a[] = " %d ";
  __builtin_printf(a, 0.5); /* { dg-warning "20: format .%d. expects argument of type .int., but argument 2 has type .double." } */
  /* { dg-begin-multiline-output "" }
   __builtin_printf(a, 0.5);
                    ^
   { dg-end-multiline-output "" } */
}
