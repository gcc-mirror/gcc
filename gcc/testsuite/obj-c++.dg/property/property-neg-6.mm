/* Check for proper declaration of @property. */
/* { dg-do compile } */

@interface Bar
{
  int iVar;
}
@property int FooBar /* { dg-error "expected ';' at end of input" } */
		     /* { dg-error "-:expected '@end' at end of input" "" { target *-*-* } .+1 } */
