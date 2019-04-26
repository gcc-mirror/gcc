/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=1 -fstrict-aliasing" } */

struct incomplete;
struct s1 { int i; };
struct s2 { double d; };

void
f (int *i, double *d, struct s1 *s1, struct s2 *s2, char *c)
{
  (char *) i;
  (char *) d;
  (char *) s1;
  (char *) s2;
  (char *) c;

  (int *) i;
  (int *) d; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (int *) s1; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (int *) s2; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (int *) c;

  (double *) i; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (double *) d;
  (double *) s1; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (double *) s2; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (double *) c;

  (struct incomplete *) i; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct incomplete *) d; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct incomplete *) s1; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct incomplete *) s2; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct incomplete *) c; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */

  (struct s1 *) i; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct s1 *) d; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct s1 *) s1;
  (struct s1 *) s2; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct s1 *) c;

  (struct s2 *) i; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct s2 *) d; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct s2 *) s1; /* { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" } */
  (struct s2 *) s2;
  (struct s2 *) c;
}
