/* Test for format attributes: test use of __attribute__.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#define DONT_GNU_PROTOTYPE
#include "format.h"

extern void tformatprintf (const char *, ...) __attribute__((format(gnu_attr_printf, 1, 2)));
extern void tformat__printf__ (const char *, ...) __attribute__((format(gnu_attr___printf__, 1, 2)));
extern void tformatscanf (const char *, ...) __attribute__((format(gnu_attr_scanf, 1, 2)));
extern void tformat__scanf__ (const char *, ...) __attribute__((format(gnu_attr___scanf__, 1, 2)));
extern void tformatstrftime (const char *) __attribute__((format(gnu_attr_strftime, 1, 0)));
extern void tformat__strftime__ (const char *) __attribute__((format(gnu_attr___strftime__, 1, 0)));
extern void tformatstrfmon (const char *, ...) __attribute__((format(strfmon, 1, 2)));
extern void tformat__strfmon__ (const char *, ...) __attribute__((format(__strfmon__, 1, 2)));
extern void t__format__printf (const char *, ...) __attribute__((__format__(gnu_attr_printf, 1, 2)));
extern void t__format____printf__ (const char *, ...) __attribute__((__format__(gnu_attr___printf__, 1, 2)));
extern void t__format__scanf (const char *, ...) __attribute__((__format__(gnu_attr_scanf, 1, 2)));
extern void t__format____scanf__ (const char *, ...) __attribute__((__format__(gnu_attr___scanf__, 1, 2)));
extern void t__format__strftime (const char *) __attribute__((__format__(gnu_attr_strftime, 1, 0)));
extern void t__format____strftime__ (const char *) __attribute__((__format__(gnu_attr___strftime__, 1, 0)));
extern void t__format__strfmon (const char *, ...) __attribute__((__format__(strfmon, 1, 2)));
extern void t__format____strfmon__ (const char *, ...) __attribute__((__format__(__strfmon__, 1, 2)));

extern char *tformat_arg (const char *) __attribute__((format_arg(1)));
extern char *t__format_arg__ (const char *) __attribute__((__format_arg__(1)));

void
foo (int i, int *ip, double d)
{
  tformatprintf ("%d", i);
  tformatprintf ("%"); /* { dg-warning "format" "attribute format printf" } */
  tformat__printf__ ("%d", i);
  tformat__printf__ ("%"); /* { dg-warning "format" "attribute format __printf__" } */
  tformatscanf ("%d", ip);
  tformatscanf ("%"); /* { dg-warning "format" "attribute format scanf" } */
  tformat__scanf__ ("%d", ip);
  tformat__scanf__ ("%"); /* { dg-warning "format" "attribute format __scanf__" } */
  tformatstrftime ("%a");
  tformatstrftime ("%"); /* { dg-warning "format" "attribute format strftime" } */
  tformat__strftime__ ("%a");
  tformat__strftime__ ("%"); /* { dg-warning "format" "attribute format __strftime__" } */
  tformatstrfmon ("%n", d);
  tformatstrfmon ("%"); /* { dg-warning "format" "attribute format strfmon" } */
  tformat__strfmon__ ("%n", d);
  tformat__strfmon__ ("%"); /* { dg-warning "format" "attribute format __strfmon__" } */
  t__format__printf ("%d", i);
  t__format__printf ("%"); /* { dg-warning "format" "attribute __format__ printf" } */
  t__format____printf__ ("%d", i);
  t__format____printf__ ("%"); /* { dg-warning "format" "attribute __format__ __printf__" } */
  t__format__scanf ("%d", ip);
  t__format__scanf ("%"); /* { dg-warning "format" "attribute __format__ scanf" } */
  t__format____scanf__ ("%d", ip);
  t__format____scanf__ ("%"); /* { dg-warning "format" "attribute __format__ __scanf__" } */
  t__format__strftime ("%a");
  t__format__strftime ("%"); /* { dg-warning "format" "attribute __format__ strftime" } */
  t__format____strftime__ ("%a");
  t__format____strftime__ ("%"); /* { dg-warning "format" "attribute __format__ __strftime__" } */
  t__format__strfmon ("%n", d);
  t__format__strfmon ("%"); /* { dg-warning "format" "attribute __format__ strfmon" } */
  t__format____strfmon__ ("%n", d);
  t__format____strfmon__ ("%"); /* { dg-warning "format" "attribute __format__ __strfmon__" } */
  tformatprintf (tformat_arg ("%d"), i);
  tformatprintf (tformat_arg ("%")); /* { dg-warning "format" "attribute format_arg" } */
  tformatprintf (t__format_arg__ ("%d"), i);
  tformatprintf (t__format_arg__ ("%")); /* { dg-warning "format" "attribute __format_arg__" } */
}
