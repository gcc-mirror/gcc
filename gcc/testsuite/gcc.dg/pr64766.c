/* PR c/64766 */
/* { dg-do compile } */

void
foo ()
{
}

void foo () = 0; /* { dg-error "is initialized like a variable|invalid initializer" } */
