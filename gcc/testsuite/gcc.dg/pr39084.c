/* { dg-do compile } */
/* { dg-options "-O2" } */

struct color { int i; }; /* { dg-message "note: originally defined here" } */
static const struct color col;
struct color * f(void)
{
    return (struct color *) &col;
}

struct color { int j; }; /* { dg-error "redefinition of" } */
