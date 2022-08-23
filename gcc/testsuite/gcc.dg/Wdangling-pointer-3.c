/* PR tree-optimization/104715 - false dangling pointer with strstr
   Vertify that using pointers that have become dangling after they were
   passed to and returned from strstr is diagnosed.
  { dg-do compile }
  { dg-options "-Wall" } */

extern char* strstr (const char*, const char*);

void sink (const void*);

void nowarn_strstr_static (const char *s)
{
  char *t1;

  {
    static const char a[] = "abc";
    t1 = strstr (a, s);
    sink (t1);
  }

  sink (t1);
}


void nowarn_strstr_lit (const char *s)
{
  char *t2;

  {
    t2 = strstr ("def", s);
    sink (t2);
  }

  sink (t2);
}


void warn_strstr_comp_lit (const char *s)
{
  char *t3;

  {
    const char *a =
      (char[]){ '1', '\0' };  // { dg-message "unnamed temporary defined here" }
    t3 = strstr (a, s);
    sink (t3);
  }

  sink (t3);            // { dg-warning "using dangling pointer 't3' to an unnamed temporary" }
}


void warn_strstr_arg (const char *s)
{
  char *t4;

  {
    char a[] = "1";     // { dg-message "'a' declared here" }
    t4 = strstr (a, s);
    sink (t4);
  }

  sink (t4);            // { dg-warning "using dangling pointer 't4' to 'a'" }
}


void warn_strstr_arg_plus_cst (const char *s)
{
  char *t5;

  {
    char a[] = "12";    // { dg-message "'a' declared here" }
    t5 = strstr (a + 1, s);
    sink (t5);
  }

  sink (t5);            // { dg-warning "using dangling pointer 't5' to 'a'" }
}


void warn_strstr_arg_plus_var (const char *s, int i)
{
  char *t6;

  {
    char a[] = "123";   // { dg-message "'a' declared here" }
    t6 = strstr (a + i, s);
    sink (t6++);
  }

  sink (t6);            // { dg-warning "using dangling pointer 't6' to 'a'" }
}
