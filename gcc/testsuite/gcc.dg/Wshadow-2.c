/* Bogus warning for a double declaration of the same extern variable,
   first at file scope, then at block scope.  PR 13129.  */

/* { dg-options "-Wshadow" } */

extern struct foo bar;
void dummy()
{
  extern struct foo bar;  /* { dg-bogus "shadows" } */
}
