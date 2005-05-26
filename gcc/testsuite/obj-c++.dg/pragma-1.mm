/* It is OK to use #pragma inside @interface body. This test checks that.  */
/* Devang Patel  <dpatel@apple.com>.  */

@interface A
{
   int p;
}
+(int) foo;
#pragma Mark foobar
-(int) bar;
@end
