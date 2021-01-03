/* Test for hiding of ivars by local variables.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface Sprite {
  int a;
}
@end

Sprite *glob;

@interface blah 
{
  Sprite* sprite;
}
@end

@implementation blah
- (Sprite *)load
{
    Sprite *sprite = 0;
    Sprite *glob = 0;    /* ok */
    return sprite;       /* { dg-warning "hides instance variable" } */
}
@end
