/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wall -Wshadow -fshow-column" } */

extern double strtod (const char *, char **);
#define UNUSED __attribute__ ((unused))

/* A built-in function may be overridden by an old-style definition
   specifying too few arguments... */
double cos ()  /* { dg-warning "shadows a built-in|number of arguments" } */ 
{
  return strtod ("nan", 0);
}

/* the right number, but the wrong type, arguments... */
double sin (foo)  /* { dg-warning "8:shadows a built-in" } */
     int foo UNUSED; /* { dg-warning "10:argument 'foo' doesn't match built-in prototype" } */
{
  return strtod ("nan", 0);
}

/* or too many arguments.  */
long double cosl (foo, bar)  /* { dg-warning "shadows a built-in|number of arguments" } */
     const char *foo UNUSED; /* { dg-warning "18:argument 'foo' doesn't match" } */
     int bar UNUSED;
{
  return strtod ("nan", 0);
}
