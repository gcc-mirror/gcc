/* PR c/9530 */
/* Contributed by Volker Reichelt. */

/* Verify that the call to 'foo' is not turned
   into a sibling call.  */

void foo(float d);

float bar(float d);

float baz(float d)
{
  foo(bar(d));
}
