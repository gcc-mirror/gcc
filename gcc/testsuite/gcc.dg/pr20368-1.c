/* ICE with -Wstrict-prototypes and typeof an undeclared function.
   Bug 20368.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wstrict-prototypes" } */

extern __typeof (f) g; /* { dg-error "'f' undeclared here \\(not in a function\\)" } */

int
f (x) /* { dg-warning "function declaration isn't a prototype" } */
     float x;
{
}
