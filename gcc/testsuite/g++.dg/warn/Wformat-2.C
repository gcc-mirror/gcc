// PR c++/84076
// { dg-do compile }
// { dg-options "-Wformat" }

struct S { ~S (); };
struct T { T (); T (const T &); };

void
foo ()
{
  S s;
  T t;
  __builtin_printf ("%s\n", s);	// { dg-warning "format '%s' expects argument of type 'char\\*', but argument 2 has type 'S'" }
  __builtin_printf ("%s\n", t);	// { dg-warning "format '%s' expects argument of type 'char\\*', but argument 2 has type 'T'" }
  __builtin_printf ("%s\n", &s);// { dg-warning "format '%s' expects argument of type 'char\\*', but argument 2 has type 'S\\*'" }
  __builtin_printf ("%s\n", &t);// { dg-warning "format '%s' expects argument of type 'char\\*', but argument 2 has type 'T\\*'" }
}
