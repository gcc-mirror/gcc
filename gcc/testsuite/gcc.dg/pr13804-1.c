/* Diagnostics for attempts to access a member not in a structure or
   union should name the type involved.  Bug 13804.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s0 { int a; };
union u0 { long b; };
typedef struct s0 S0;
typedef union u0 U0;

struct s0 x0;
S0 x1;
union u0 x2;
U0 x3;
struct s0 *x4;
union u0 *x5;

void
f (void)
{
  x0.c; /* { dg-error "'struct s0' has no member named 'c'" } */
  x1.c; /* { dg-error "'S0' has no member named 'c'" } */
  x2.c; /* { dg-error "'union u0' has no member named 'c'" } */
  x3.c; /* { dg-error "'U0' has no member named 'c'" } */
  x4->c; /* { dg-error "'struct s0' has no member named 'c'" } */
  x5->c; /* { dg-error "'union u0' has no member named 'c'" } */
}
