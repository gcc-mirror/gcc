/* Verify that calls to non-modifying built-ins aren't considered
   potentially modifying.
   { dg-do compile }
   { dg-options "-std=gnu17 -O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

void* alloca (size_t);
void* calloc (size_t, size_t);
void* malloc (size_t);
int printf (const char *, ...);
int scanf (const char *, ...);
int sprintf (char *, const char *, ...);
int snprintf (char *, size_t, const char *, ...);
int puts (const char *);
char* strcpy (char*, const char*);
size_t strlen (const char*);

void noproto ();

void sink (int, ...);

extern char a[];

void nowarn_noproto (const char *fmt)
{
  int i;
  noproto (&i);
  sink (i);
}

void nowarn_scanf (const char *fmt)
{
  int i;
  scanf ("%i", &i);
  sink (i);
}

void test_puts_sprintf_alloca (const char *fmt)
{
  char *p;
  {
    p = alloca (8);
    sprintf (a, fmt, p);                // fmt might contain %n
    puts (p);
  }

  {
    p = alloca (8);
    snprintf (0, 0, fmt, p);            // same as above
    puts (p);
  }
}

void test_puts_alloca (const char *s)
{
  char *p = alloca (8);

  {
    char a[] = "foo";
    puts (a);
  }

  puts (p);                             // { dg-warning "-Wuninitialized" }

  {
    p = alloca (strlen (s) + 1);
    strcpy (p, s);
    puts (p);
  }

  {
    /* Verify that the puts() calls above isn't considered to have
       potentially modified *P, and same for the one below.  */
    p = alloca (strlen (s));
    puts (p);                           // { dg-warning "-Wuninitialized" }
    puts (p + 1);                       // { dg-warning "-Wuninitialized" }
  }
}


void test_puts_malloc (const char *s, const char *t)
{
  char *p;

  {
    p = malloc (strlen (s) + 1);
    strcpy (p, s);
    puts (p);
  }

  {
    p = malloc (strlen (t));
    puts (p);                           // { dg-warning "-Wuninitialized" }
  }
}


void test_puts_vla (const char *s, const char *t)
{
  {
    char a[strlen (s) + 1];
    strcpy (a, s);
    puts (a);
  }

  {
    char b[strlen (t)];
    puts (b);                           // { dg-warning "-Wuninitialized" }
  }
}


void test_printf_puts (const char *s)
{
  char *p = __builtin_malloc (1);

  printf ("%s", s);

  puts (p);                             // { dg-warning "-Wuninitialized" }
}
