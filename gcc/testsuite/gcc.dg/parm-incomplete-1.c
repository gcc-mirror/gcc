/* Test warnings and errors for incomplete parameter types.  Should
   not be warned for in declarations that are not definitions: bug
   17881.  Void types may be a special case, especially for unnamed
   parameters and when qualified or with a storage class specifier;
   see C90 6.5.4.3, DR#017 Q14, C90 TC1, DR#157, C99 J.2 (referencing
   C99 6.7.5.3); the precise rules are unclear.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s;
void f (struct s);
void (*g)(struct s);
struct t { void (*p)(struct s); };

struct s { int b; };
void h (struct s x) { }

void j(struct t2); /* { dg-warning "'struct t2' declared inside parameter list" } */
/* { dg-warning "its scope is only" "explanation" { target *-*-* } 19 } */

union u;

void v(union u x) { } /* { dg-error "parameter 1 \\('x'\\) has incomplete type" } */

void p(void x); /* { dg-warning "parameter 1 \\('x'\\) has void type" } */

void q(const void x); /* { dg-warning "parameter 1 \\('x'\\) has void type" } */
