#include <stdio.h>

/* Solaris needs this for <unistd.h> to declare getpass.  */
#define __EXTENSIONS__
#include <unistd.h>

#include <string.h>

/* Declare getpass, in case unistd doesn't declare it.
   Parenthesize it, in case it's a macro.
   Don't use a prototype, to avoid const mismatches.  */
extern char *(getpass) ();

char test_1 (FILE *logfile)
{
  char *password = getpass (">"); /* { dg-message "\\(1\\) sensitive value acquired here" } */
  fprintf (logfile, "got password %s\n", password); /* { dg-warning "sensitive value 'password' written to output file \\\[CWE-532\\\]" "warning" } */
  /* { dg-message "\\(2\\) sensitive value 'password' written to output file; acquired at \\(1\\)" "event" { target *-*-* } .-1 } */
}

char test_2 (FILE *logfile, int i)
{
  char *password = getpass (">"); /* { dg-message "\\(1\\) sensitive value acquired here" } */
  fprintf (logfile, "got password[%i]: %s\n", i, password); /* { dg-warning "sensitive value 'password' written to output file \\\[CWE-532\\\]" } */
  /* { dg-message "\\(2\\) sensitive value 'password' written to output file; acquired at \\(1\\)" "event" { target *-*-* } .-1 } */
}

char test_3 (FILE *logfile)
{
  char *password = getpass (">"); /* { dg-message "\\(1\\) sensitive value acquired here" } */
  printf ("got password %s\n", password); /* { dg-warning "sensitive value 'password' written to output file \\\[CWE-532\\\]" "warning" } */
  /* { dg-message "\\(2\\) sensitive value 'password' written to output file; acquired at \\(1\\)" "event" { target *-*-* } .-1 } */
}

char test_4 (FILE *logfile)
{
  char *password = getpass (">"); /* { dg-message "\\(1\\) sensitive value acquired here" } */
  fwrite (password, strlen (password), 1, logfile); /* { dg-warning "sensitive value 'password' written to output file \\\[CWE-532\\\]" "warning" } */
  /* { dg-message "\\(2\\) sensitive value 'password' written to output file; acquired at \\(1\\)" "event" { target *-*-* } .-1 } */
}

static void called_by_test_5 (const char *value)
{
  printf ("%s", value); /* { dg-warning "sensitive value 'value' written to output file \\\[CWE-532\\\]" } */
}

char test_5 (FILE *logfile)
{
  char *password = getpass (">");
  called_by_test_5 (password); /* { dg-message "passing sensitive value 'password' in call to 'called_by_test_5' from 'test_5'" } */
}

static char *called_by_test_6 (void)
{
  return getpass (">"); /* { dg-message "sensitive value acquired here" } */
}

char test_6 (FILE *logfile)
{
  char *password = called_by_test_6 (); /* { dg-message "returning sensitive value to 'test_6' from 'called_by_test_6'" } */
  printf ("%s", password); /* { dg-warning "sensitive value 'password' written to output file \\\[CWE-532\\\]" } */
}

/* TODO: strdup etc, strcpy, memcpy, etc.  */
