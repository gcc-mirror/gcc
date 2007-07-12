/* Test that "typeof(struct foo)" and similar as declaration
   specifiers act like typedef.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

typeof(struct foo); /* { dg-warning "useless type name in empty declaration" } */

struct bar { int a; } x;

typeof(x); /* { dg-warning "useless type name in empty declaration" } */
