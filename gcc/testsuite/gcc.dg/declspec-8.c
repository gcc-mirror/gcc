/* Test declaration specifiers.  Test checks on storage class
   specifiers and function specifiers in empty declarations.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

/* The constraints on storage class specifiers and function specifiers
   must be met for empty declarations where they are useless.  Thus
   there may be only one storage class specifier (C90 6.5.1, C99
   6.7.1#2) and "inline" must not be used because the declaration is
   not that of an identifier for a function (C99 6.7.4#1), and
   "register" and "auto" must not be used at file scope (C90 6.7, C99
   6.9#2).  */

static static struct s; /* { dg-error "duplicate 'static'" } */
/* { dg-warning "useless storage class specifier in empty declaration" "static static" { target *-*-* } 15 } */

static extern struct t; /* { dg-error "multiple storage classes in declaration specifiers" } */
/* { dg-warning "useless storage class specifier in empty declaration" "static extern" { target *-*-* } 18 } */

inline union u; /* { dg-error "'inline' in empty declaration" } */

auto struct v; /* { dg-error "'auto' in file-scope empty declaration" } */

register struct w; /* { dg-error "'register' in file-scope empty declaration" } */

void
f (void)
{
  auto union p; /* { dg-warning "useless storage class specifier in empty declaration" } */
  register struct q; /* { dg-warning "useless storage class specifier in empty declaration" } */
}
