/* PR objc/29195 */
/* Test that array decls are changed to a pointer type
   correctly and make sure we don't crash because the
   decl was not relayed out.   */

@ implementation NGActiveSocket 
+ (void) socketPair:(int [2])
     _pair 
{
  _pair[0] = 0;
}
@end
