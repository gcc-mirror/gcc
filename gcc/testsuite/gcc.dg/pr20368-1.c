/* ICE with -Wstrict-prototypes and typeof an undeclared function.
   Bug 20368.  */
/* { dg-do compile } */
/* { dg-options "-Wstrict-prototypes" } */

extern __typeof (f) g; /* { dg-error "error: 'f' undeclared here \\(not in a function\\)" } */

int
f (x)
     float x; /* { dg-warning "warning: function declaration isn't a prototype" } */
{
}
