/* Test looking up fields in superclasses in the context of write-barriers
   (where component references get rewritten).  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */
/* { dg-skip-if "GC API is an error from Darwin16." { *-*-darwin1[6-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-options "-fobjc-gc" } */
/* { dg-prune-output "cc1objplus: warning: '-fobjc-gc' is ignored for '-fgnu-runtime'" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include "../objc-obj-c++-shared/runtime.h"

@class MyWindow;

@interface MyDocument : TestsuiteObject {
    MyWindow *_window;
}
@end

@interface MyFileDocument : MyDocument {
     struct {
        unsigned int autoClose:1;
        unsigned int openForUI:1;
        unsigned int isClosing:1;
        unsigned int needsDiskCheck:1;
        unsigned int isWritable:1;
        unsigned int representsFileOnDisk:1;
        unsigned int RESERVED:26;
    } _fdFlags;
}
@end

@interface MyTextFileDocument : MyFileDocument {
    TestsuiteObject *_textStorage;
    struct __tfdFlags {
        unsigned int immutable:1;
        unsigned int lineEnding:2;
        unsigned int isClosing:1;
        unsigned int settingsAreSet:1;
        unsigned int usesTabs:1;
        unsigned int isUTF8WithBOM:1;
        unsigned int wrapsLines:1;
        unsigned int usingDefaultLanguage:1;
        unsigned int RESERVED:23;
    } _tfdFlags;
    int _tabWidth;
    int _indentWidth;
}
@end

@interface MyRTFFileDocument : MyTextFileDocument
- (BOOL)readFromFile:(const char *)fileName ofType:(const char *)type;
@end

@implementation MyRTFFileDocument
- (BOOL)readFromFile:(const char *)fileName ofType:(const char *)type {
        if (_textStorage && fileName) {
            [_textStorage free];
	    return YES;
        } else if (type) {
            _textStorage = [MyRTFFileDocument new];
	    return NO;
        }
   return (fileName && type);
}
@end
