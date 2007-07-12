/* Test for handling of tags.  "const struct foo;" and similar does
   not redeclare an existing tag.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* Plain "struct s;" always declares a tag: the same as one declared
   in that scope, or shadowing one from an outer scope.  */
struct s0;
struct s0 { int a; };
struct s0;
void f (void) { struct s0; }

/* A declaration with a qualifier or storage class specifier declares
   the tag if no other declaration of it is visible.  */
const union u0; /* { dg-warning "useless type qualifier in empty declaration" } */
union u0 { long b; };

extern struct s1; /* { dg-warning "useless storage class specifier in empty declaration" } */

/* But if a declaration of the tag is visible, whether at the same
   scope or an outer scope, the declaration specifies the same type as
   the previous declaration and does not redeclare the tag (C99
   6.7.2.3#8).  Thus, as it does not declare a declarator, a tag or
   the members of an enumeration, it is a constraint violation.  */

struct s2 { char x; };
const struct s2; /* { dg-error "empty declaration with type qualifier does not redeclare tag" } */

union u1;
extern union u1; /* { dg-error "empty declaration with storage class specifier does not redeclare tag" } */

union u2 { long b; };
void g(void) { const union u2; } /* { dg-error "empty declaration with type qualifier does not redeclare tag" } */

/* And it does not redeclare the tag either if the outer tag is the
   wrong kind of tag.  This also yields an error for the reference to
   the wrong kind of tag in addition to the pedwarn for the empty
   declaration.  */

union u3 { float v; };
void h(void) { const struct u3; } /* { dg-error "'u3' defined as wrong kind of tag" } */
/* { dg-error "empty declaration with type qualifier does not redeclare tag" "wrong tag empty" { target *-*-* } 42 } */

/* However, such useless specifiers are OK if the contents of the tag
   are being defined, or shadowed in an inner scope with the contents
   included in the shadowing.  */

struct s3;
const struct s3 { int a; }; /* { dg-warning "useless type qualifier in empty declaration" } */

union u4;
extern union u4 { int z; }; /* { dg-warning "useless storage class specifier in empty declaration" } */

enum e0 { E0 };
void i(void) { const enum e0 { E1 }; } /* { dg-warning "useless type qualifier in empty declaration" } */

union u5 { int p; };
void j(void) { extern struct u5 { int q; }; } /* { dg-warning "useless storage class specifier in empty declaration" } */
