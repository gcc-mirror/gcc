/* PR c++/104148 */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

char *foo (const char *) __attribute__((format_arg(1)));
void bar (const char *, ...) __attribute__((format(printf, 1, 2)));

void
baz (int x)
{
  bar ("%ld", x);			/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar (x ? "%ld" : "%ld", x);		/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar (x ? "%ld" : "%lld", x);		/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
					/* { dg-warning "format '%lld' expects argument of type 'long long int', but argument 2 has type 'int'" "" { target *-*-* } .-1 } */
  bar (foo ("%ld"), x);			/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar (x ? foo ("%ld") : "%ld", x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar (x ? foo ("%ld") : "%lld", x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
					/* { dg-warning "format '%lld' expects argument of type 'long long int', but argument 2 has type 'int'" "" { target *-*-* } .-1 } */
  bar (foo (x ? "%ld" : "%ld"), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar (foo (x ? "%ld" : "%lld"), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
					/* { dg-warning "format '%lld' expects argument of type 'long long int', but argument 2 has type 'int'" "" { target *-*-* } .-1 } */
  bar (("%ld"), x);			/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar ((x ? "%ld" : "%ld"), x);		/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar ((x ? "%ld" : "%lld"), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
					/* { dg-warning "format '%lld' expects argument of type 'long long int', but argument 2 has type 'int'" "" { target *-*-* } .-1 } */
  bar ((foo ("%ld")), x);		/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar ((x ? foo ("%ld") : "%ld"), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar ((x ? foo ("%ld") : "%lld"), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
					/* { dg-warning "format '%lld' expects argument of type 'long long int', but argument 2 has type 'int'" "" { target *-*-* } .-1 } */
  bar ((foo (x ? "%ld" : "%ld")), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
  bar ((foo (x ? "%ld" : "%lld")), x);	/* { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" } */
					/* { dg-warning "format '%lld' expects argument of type 'long long int', but argument 2 has type 'int'" "" { target *-*-* } .-1 } */
}
