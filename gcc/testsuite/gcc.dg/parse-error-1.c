/* PR c/28649 */
/* { dg-do compile } */

void foo()
{
   +;  /* { dg-error "expected expression" } */
   +;  /* { dg-error "expected expression" } */
}

int +;  /* { dg-error "expected identifier" } */
