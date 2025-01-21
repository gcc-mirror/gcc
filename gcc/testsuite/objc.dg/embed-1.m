/* PR objc++/118586 */
/* { dg-do compile } */

@interface Foo
+ (int) bar: (int) firstNumber, int secondNumber, ...;
@end

void
baz (void)
{
  [Foo bar: 1, 2,
#embed __FILE__
       , -1];
}
