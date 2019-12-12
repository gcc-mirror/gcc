/* PR tree-optimization/83431 -Wformat-truncation may incorrectly report
   truncation
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern int snprintf (char*, size_t, const char*, ...);
extern char* strcpy (char*, const char*);

struct S
{
  char a9[9];
  char a5[5];
  int x;
};


void test_assign_nowarn (struct S* s)
{
  int i = 0;

  {
    char a9[9] = "1234";
    snprintf (s[i].a5, sizeof (s[i].a5), "%s", a9);         /* { dg-bogus "\\\[-Wformat-truncation]" } */
  }

  {
    ++i;
    char a8[8] = "123";
    snprintf (s[i].a5, sizeof (s[i].a5), "%s\n", a8);       /* { dg-bogus "\\\[-Wformat-truncation]" } */
  }

  {
    ++i;
    char a7[7] = "12";
    snprintf (s[i].a5, sizeof (s[i].a5), "[%s]", a7);       /* { dg-bogus "\\\[-Wformat-truncation]" } */
  }

  {
    ++i;
    char a6[6] = "1";
    snprintf (s[i].a5, sizeof (s[i].a5), "[%s]\n", a6);     /* { dg-bogus "\\\[-Wformat-truncation]" } */
  }
}


void test_strcpy_nowarn (struct S* s)
{
  int i = 0;

  strcpy (s[i].a9, "1234");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s", s[i].a9);

  ++i;
  strcpy (s[i].a9, "123");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s\n", s[i].a9);    /* { dg-bogus "\\\[-Wformat-truncation]" } */

  ++i;
  strcpy (s[i].a9, "12");
  snprintf (s[i].a5, sizeof (s[i].a5), "[%s]", s[i].a9);    /* { dg-bogus "\\\[-Wformat-truncation]" } */

  ++i;
  strcpy (s[i].a9, "1");
  snprintf (s[i].a5, sizeof (s[i].a5), "[%s]\n", s[i].a9);  /* { dg-bogus "\\\[-Wformat-truncation]" } */
}


void test_warn (struct S* s)
{
  int i = 0;
  strcpy (s[i].a9, "12345678");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s", s[i].a9);      /* { dg-warning "'%s' directive output truncated writing 8 bytes into a region of size 5" } */

  ++i;
  strcpy (s[i].a9, "1234567");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s", s[i].a9);      /* { dg-warning "'%s' directive output truncated writing 7 bytes into a region of size 5" } */

  ++i;
  strcpy (s[i].a9, "123456");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s", s[i].a9);      /* { dg-warning "'%s' directive output truncated writing 6 bytes into a region of size 5" } */

  ++i;
  strcpy (s[i].a9, "12345");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s", s[i].a9);      /* { dg-warning "'snprintf' output truncated before the last format character" } */

  ++i;
  strcpy (s[i].a9, "1234");
  snprintf (s[i].a5, sizeof (s[i].a5), "%s\n", s[i].a9);    /* { dg-warning "output truncated before the last format character" } */

  ++i;
  strcpy (s[i].a9, "123");
  snprintf (s[i].a5, sizeof (s[i].a5), ">%s<", s[i].a9);    /* { dg-warning "output truncated before the last format character" } */
}
