/* PR c/54391 - transparent_union typedef'ing inconsistent
   { dg-do compile }
   { dg-options "-Wall" } */

typedef union m30_u m30_t;

union __attribute__((transparent_union)) m30_u {
  int u;
};

double make_double (m30_t);

double f (void)
{
  int bar = 17;
  return make_double (bar);
}
