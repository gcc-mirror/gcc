/* { dg-do compile } */
/* { dg-options "-Wall -Wshadow" } */

extern double strtod (const char *, char **);
#define UNUSED __attribute__ ((unused))

/* A built-in function may be overridden by an old-style definition
   specifying too few arguments... */
double nan ()
{
  return strtod ("nan", 0);  /* { dg-warning "shadowing built-in" } */
}

/* the right number, but the wrong type, arguments... */
float nanf (foo)
     int foo UNUSED;
{
  return strtod ("nan", 0);  /* { dg-warning "shadowing built-in" } */
}

/* or too many arguments.  */
long double nanl (foo, bar)
     const char *foo UNUSED;
     int bar UNUSED;
{
  return strtod ("nan", 0);  /* { dg-warning "shadowing built-in" } */
}
