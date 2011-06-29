/* Test for ivar access inside of class methods.  It should be allowed
   (with a warning), but only if no other declarations with the same
   name are seen.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@interface Sprite: TestsuiteObject {
  int sprite, spree;
}
+ (void)setFoo:(int)foo;
+ (void)setSprite:(int)sprite;
- (void)setFoo:(int)foo;
- (void)setSprite:(int)sprite;
@end

int spree = 23;

@implementation Sprite
+ (void)setFoo:(int)foo {
  sprite = foo;  /* { dg-warning "instance variable .sprite. accessed in class method" } */
  spree = foo;
}
+ (void)setSprite:(int)sprite {
  int spree;
  sprite = 15;
  spree = 17;
  ((Sprite *)self)->sprite = 16;   /* NB: This is how one _should_ access */
  ((Sprite *)self)->spree = 18;    /* ivars from within class methods!    */
}
- (void)setFoo:(int)foo {
  sprite = foo;
  spree = foo;
}
- (void)setSprite:(int)sprite {
  int spree;
  sprite = 15;  /* { dg-warning "local declaration of .sprite. hides instance variable" } */
  self->sprite = 16;
  spree = 17;  /* { dg-warning "local declaration of .spree. hides instance variable" } */
  self->spree = 18;
}   
@end
