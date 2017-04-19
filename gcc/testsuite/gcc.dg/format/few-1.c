/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wformat" } */

int f(int *ip, char *cp)
{
	__builtin_printf ("%*.*s");
/* { dg-warning "field width specifier '\\*' expects a matching 'int' argument" "width" { target *-*-* } .-1 } */
/* { dg-warning "field precision specifier '\\.\\*' expects a matching 'int' argument" "precision" { target *-*-* } .-2 } */
/* { dg-warning "format '%s' expects a matching 'char \\*' argument" "format" { target *-*-* } .-3 } */
	__builtin_printf ("%*.*s", ip, *cp);
/* { dg-warning "field width specifier '\\*' expects argument of type 'int'" "width" { target *-*-* } .-1 } */
/* { dg-warning "format '%s' expects a matching 'char \\*' argument" "format" { target *-*-* } .-2 } */
	__builtin_printf ("%s %i", ip, ip);
/* { dg-warning "format '%s' expects argument of type 'char \\*'" "char" { target *-*-* } .-1 } */
/* { dg-warning "format '%i' expects argument of type 'int'" "int" { target *-*-* } .-2 } */
	__builtin_printf ("%s %i", cp);
/* { dg-warning "format '%i' expects a matching 'int' argument" "" { target *-*-* } .-1 } */
	__builtin_printf ("%lc");
/* { dg-warning "format '%lc' expects a matching 'wint_t' argument" "" { target *-*-* } .-1 } */
	__builtin_printf ("%lc", cp);
/* { dg-warning "format '%lc' expects argument of type 'wint_t'" "" { target *-*-* } .-1 } */
	__builtin_scanf ("%s");
/* { dg-warning "format '%s' expects a matching 'char \\*' argument" "" { target *-*-* } .-1 } */
	__builtin_scanf ("%i", cp);
/* { dg-warning "format '%i' expects argument of type 'int \\*'" "" { target *-*-* } .-1 } */
	__builtin_scanf ("%lc");
/* { dg-warning "format '%lc' expects a matching 'wchar_t \\*' argument" "" { target *-*-* } .-1 } */
	__builtin_scanf ("%lc", cp);
/* { dg-warning "format '%lc' expects argument of type 'wchar_t \\*'" "" { target *-*-* } .-1 } */
}
