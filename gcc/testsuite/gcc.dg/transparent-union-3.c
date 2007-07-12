/* Test for ICEs on invalid transparent unions (empty or no named
   members).  Bug 21213.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

enum e { A };

union __attribute__((__transparent_union__)) ue1 { enum e; }; /* { dg-warning "declaration does not declare anything" } */
/* { dg-warning "union cannot be made transparent" "" { target *-*-* } 9 } */
union ue2 { enum e; } __attribute__((__transparent_union__)); /* { dg-warning "declaration does not declare anything" } */
/* { dg-warning "union cannot be made transparent" "" { target *-*-* } 11 } */

union __attribute__((__transparent_union__)) ui1 { int; }; /* { dg-warning "declaration does not declare anything" } */
/* { dg-warning "union cannot be made transparent" "" { target *-*-* } 14 } */
union ui2 { int; } __attribute__((__transparent_union__)); /* { dg-warning "declaration does not declare anything" } */
/* { dg-warning "union cannot be made transparent" "" { target *-*-* } 16 } */

union __attribute__((__transparent_union__)) u1 { };
/* { dg-warning "union cannot be made transparent" "" { target *-*-* } 19 } */
union u2 { } __attribute__((__transparent_union__));
/* { dg-warning "union cannot be made transparent" "" { target *-*-* } 21 } */
