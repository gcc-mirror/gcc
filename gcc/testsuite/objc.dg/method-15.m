/* Test if prior method lookup at method @implementation time is not
   overly aggressive, leading to methods being found in other classes.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include <objc/objc.h>
#include "../objc-obj-c++-shared/runtime.h"

@class NSString;

@protocol NSMenuItem
+ (void)setUsesUserKeyEquivalents:(BOOL)flag;
+ (BOOL)usesUserKeyEquivalents;
@end

@interface NSMenuItem : TestsuiteObject <NSMenuItem> {
  @private
  id _menu;
}
@end

@interface NSResponder : TestsuiteObject <NSMenuItem>
{
  id _nextResponder;
}
@end

@interface TestsuiteObject(NSMenuValidation)
- (BOOL)validateMenuItem:(id <NSMenuItem>)menuItem;
@end

@interface NSResponder (NSStandardKeyBindingMethods)
- (void)insertText:(id)insertString;
- (void)doCommandBySelector:(SEL)aSelector;
@end

@interface NSView : NSResponder
{
  id _superview;
  id _subviews;
}
@end

@interface SKTGraphicView : NSView {
    @private
    float _gridSpacing;
}
@end

@implementation SKTGraphicView
- (BOOL)validateMenuItem:(NSMenuItem *)item {
  return (BOOL)1;
}
- (void)insertText:(NSString *)str {
}
@end
