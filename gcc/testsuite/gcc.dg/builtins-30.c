/* { dg-do compile } */
/* { dg-options "-Wall -Wshadow" } */

extern double strtod (const char *, char **);
#define UNUSED __attribute__ ((unused))

/* A built-in function may be overridden by an old-style definition
   specifying too few arguments... */
double cos ()  /* { dg-warning "shadows a built-in" } */
{
  /* { dg-warning "number of arguments doesn't match built-in prototype" "built-in" { target *-*-* } 10 } */
  return strtod ("nan", 0);
}

/* the right number, but the wrong type, arguments... */
double sin (foo)  /* { dg-warning "shadows a built-in" } */
     int foo UNUSED;
{  /* { dg-warning "argument 'foo' doesn't match built-in prototype" } */
  return strtod ("nan", 0);
}

/* or too many arguments.  */
long double cosl (foo, bar)  /* { dg-warning "shadows a built-in" } */
     const char *foo UNUSED;
     int bar UNUSED;
{  /* { dg-warning "number of arguments doesn't match built-in prototype" } */
  /* { dg-warning "argument 'foo' doesn't match built-in prototype" "foo" { target *-*-* } 26 } */
  return strtod ("nan", 0);
}
