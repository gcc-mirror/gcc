/* Test diagnostics for empty structures and unions.  Test with no
   special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s0 {};
union u0 {};
struct s1 { int : 1; };
union u1 { int : 1; };
