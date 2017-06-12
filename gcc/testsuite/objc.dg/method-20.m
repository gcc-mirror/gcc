/* { dg-do compile }  */

/* We used to crash after we found the type for int[m] was declared as invalid. */
/* PR objc/29197 */

@ implementation NGActiveSocket
+ (void) socketPair:(int[m]) _pair {} /* { dg-error "" } */
       /* { dg-warning "" "" { target *-*-* } .-1 } */
@end
