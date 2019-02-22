/* PR tree-optimization/80523 - -Wformat-overflow doesn't consider
   -fexec-charset
   { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-options "-O2 -Wall -Wno-format -Wformat-overflow -fexec-charset=IBM1047 -ftrack-macro-expansion=0" } */

char buf[1];
void sink (void*);

#define T(...) (__builtin_sprintf (buf + 1, __VA_ARGS__), sink (buf))

/* Exercise all special C and POSIX characters.  */

void test_characters ()
{
  T ("%%");           /* { dg-warning ".%%. directive writing 1 byte" } */

  T ("%A",    0.0);   /* { dg-warning ".%A. directive writing between 6 and 20 " } */
  T ("%a",    0.0);   /* { dg-warning ".%a. directive writing between 6 and 20 " } */

  T ("%C",   L'a');   /* { dg-warning ".%C. directive writing up to 6 bytes" } */
  T ("%c",    'a');   /* { dg-warning ".%c. directive writing 1 byte" } */

  T ("%d",     12);   /* { dg-warning ".%d. directive writing 2 bytes" } */
  T ("% d",    12);   /* { dg-warning ".% d. directive writing 3 bytes" } */
  T ("%-d",   123);   /* { dg-warning ".%-d. directive writing 3 bytes" } */
  T ("%+d",  1234);   /* { dg-warning ".%\\+d. directive writing 5 bytes" } */
  T ("%'d",  1234);   /* { dg-warning ".%'d. directive writing 5 bytes" "bug 80535" { xfail *-*-* } } */
  T ("%1$d", 2345);   /* { dg-warning ".%1\\\$d. directive writing 4 bytes" } */

  /* Verify that digits are correctly interpreted as width and precision.  */
  T ("%0d", 12345);   /* { dg-warning ".%0d. directive writing 5 bytes" } */
  T ("%1d", 12345);   /* { dg-warning ".%1d. directive writing 5 bytes" } */
  T ("%2d", 12345);   /* { dg-warning ".%2d. directive writing 5 bytes" } */
  T ("%3d", 12345);   /* { dg-warning ".%3d. directive writing 5 bytes" } */
  T ("%4d", 12345);   /* { dg-warning ".%4d. directive writing 5 bytes" } */
  T ("%5d", 12345);   /* { dg-warning ".%5d. directive writing 5 bytes" } */
  T ("%6d", 12345);   /* { dg-warning ".%6d. directive writing 6 bytes" } */
  T ("%7d", 12345);   /* { dg-warning ".%7d. directive writing 7 bytes" } */
  T ("%8d", 12345);   /* { dg-warning ".%8d. directive writing 8 bytes" } */
  T ("%9d", 12345);   /* { dg-warning ".%9d. directive writing 9 bytes" } */

  T ("%.0d", 12345);  /* { dg-warning ".%.0d. directive writing 5 bytes" } */
  T ("%.1d", 12345);  /* { dg-warning ".%.1d. directive writing 5 bytes" } */
  T ("%.2d", 12345);  /* { dg-warning ".%.2d. directive writing 5 bytes" } */
  T ("%.3d", 12345);  /* { dg-warning ".%.3d. directive writing 5 bytes" } */
  T ("%.4d", 12345);  /* { dg-warning ".%.4d. directive writing 5 bytes" } */
  T ("%.5d", 12345);  /* { dg-warning ".%.5d. directive writing 5 bytes" } */
  T ("%.6d", 12345);  /* { dg-warning ".%.6d. directive writing 6 bytes" } */
  T ("%.7d", 12345);  /* { dg-warning ".%.7d. directive writing 7 bytes" } */
  T ("%.8d", 12345);  /* { dg-warning ".%.8d. directive writing 8 bytes" } */
  T ("%.9d", 12345);  /* { dg-warning ".%.9d. directive writing 9 bytes" } */

  T ("%hhd",    12);   /* { dg-warning ".%hhd. directive writing 2 bytes" } */
  T ("%hd",    234);   /* { dg-warning ".%hd. directive writing 3 bytes" } */

  {
    const __PTRDIFF_TYPE__ i = 3456;
    T ("%jd",   i);  /* { dg-warning ".%jd. directive writing 4 bytes" } */
  }

  T ("%ld",  45678L);  /* { dg-warning ".%ld. directive writing 5 bytes" } */

  {
    const __PTRDIFF_TYPE__ i = 56789;
    T ("%td",   i);  /* { dg-warning ".%td. directive writing 5 bytes" } */
  }

  {
    const __SIZE_TYPE__ i = 67890;
    T ("%zd",   i);  /* { dg-warning ".%zd. directive writing 5 bytes" } */
  }

  T ("%E",    0.0);   /* { dg-warning ".%E. directive writing 12 bytes" } */
  T ("%e",    0.0);   /* { dg-warning ".%e. directive writing 12 bytes" } */
  T ("%F",    0.0);   /* { dg-warning ".%F. directive writing 8 bytes" } */
  T ("%f",    0.0);   /* { dg-warning ".%f. directive writing 8 bytes" } */
  T ("%G",    0.0);   /* { dg-warning ".%G. directive writing 1 byte" } */
  T ("%g",    0.0);   /* { dg-warning ".%g. directive writing 1 byte" } */

  T ("%i",     123);  /* { dg-warning ".%i. directive writing 3 bytes" } */

  {
    int n;

    T ("%n",    &n);  /* { dg-warning "writing a terminating nul" } */
    T ("%nH",   &n);  /* { dg-warning ".H. directive writing 1 byte" } */
  }

  T ("%o",     999);  /* { dg-warning ".%o. directive writing 4 bytes" } */
  T ("%#o",    999);  /* { dg-warning ".%#o. directive writing 5 bytes" } */

  T ("%x",    1234);  /* { dg-warning ".%x. directive writing 3 bytes" } */
  T ("%#X",   1235);  /* { dg-warning ".%#X. directive writing 5 bytes" } */

  T ("%S",    L"1");  /* { dg-warning ".%S. directive writing up to 6 bytes" } */
  T ("%ls",  L"12");  /* { dg-warning ".%ls. directive writing up to 12 bytes" } */
  T ("%-s",    "1");  /* { dg-warning ".%-s. directive writing 1 byte" } */

  /* Verify that characters in the source character set appear in
     the text of the warning unchanged (i.e., not as their equivalents
     in the execution character set on the target).  The trailing %%
     disables sprintf->strcpy optimization.  */
  T ("ABCDEFGHIJ%%");   /* { dg-warning ".ABCDEFGHIJ. directive writing 10 bytes" } */
  T ("KLMNOPQRST%%");   /* { dg-warning ".KLMNOPQRST. directive writing 10 bytes" } */
  T ("UVWXYZ%%");       /* { dg-warning ".UVWXYZ. directive writing 6 bytes" } */

  T ("abcdefghij%%");   /* { dg-warning ".abcdefghij. directive writing 10 bytes" } */
  T ("klmnopqrst%%");   /* { dg-warning ".klmnopqrst. directive writing 10 bytes" } */
  T ("uvwxyz%%");       /* { dg-warning ".uvwxyz. directive writing 6 bytes" } */
}

#undef T
#define T(...) (__builtin_sprintf (d, __VA_ARGS__), sink (d))

void test_width_and_precision_out_of_range (char *d)
{
  /* The range here happens to be a property of the compiler, not
     one of the target.  */
  T ("%9223372036854775808i", 0);    /* { dg-warning "width out of range" "first" } */
  /* { dg-warning "exceeds .INT_MAX." "second" { target *-*-* } .-1 } */
  T ("%.9223372036854775808i", 0);   /* { dg-warning "precision out of range" "first" } */
  /* { dg-warning "exceeds .INT_MAX." "second" { target *-*-* } .-1 } */

  /* The following is diagnosed by -Wformat (disabled here).  */
  /* T ("%9223372036854775808$i", 0); */
}

/* Verify that an excessively long directive is truncated and the truncation
   is indicated by three trailing dots in the text of the warning.  */

void test_overlong_plain_string ()
{
  static const char longfmtstr[] =
    "0123456789012345678901234567890123456789012345678901234567890123456789%%";

  char d[1];
  T (longfmtstr);   /* { dg-warning ".0123\[0-9\]\*\.\.\.. directive writing 70 bytes" } */
}
