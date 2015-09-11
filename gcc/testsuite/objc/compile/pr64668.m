typedef struct objc_object {} *id;
typedef struct _NSZone NSZone;
@class NSCoder;
@protocol NSObject - (Class) class;
@end @protocol NSCopying - (id) copyWithZone: (NSZone*)zone;
@end @protocol NSMutableCopying - (id) mutableCopyWithZone: (NSZone*)zone;
@end @protocol NSCoding - (void) encodeWithCoder: (NSCoder*)aCoder;
@end  @interface NSObject <NSObject> {}
@end typedef double NSTimeInterval;
@interface NSString :NSObject <NSCoding, NSCopying, NSMutableCopying> + (id) string;
@end @interface NSConstantString : NSString {}
@end @class NSMutableArray, NSMutableDictionary, NSMutableData, NSData, NSString;
@interface NSBundle : NSObject {}
enum { NSMixedState = -1, NSOffState = 0, NSOnState = 1 };
@end @class NSWindow;
@interface IBInspector : NSObject { id object; }
@end @interface GormScrollViewAttributesInspector : IBInspector { id verticalScroll; id horizontalScroll; }
@end @implementation GormScrollViewAttributesInspector - init {}
- (void) verticalSelected: (id)sender { [super ok: sender]; [object setHasVerticalScroller: ([verticalScroll state] == NSOnState)]; }
- (void) horizontalSelected: (id)sender { [super ok: sender]; [object setHasHorizontalScroller: ([horizontalScroll state] == NSOnState)]; }
