/* All calls must be properly stubified.  */
/* Testcase extracted from TextEdit:Document.m.  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mdynamic-no-pic -fdump-rtl-jump -mmacosx-version-min=10.4" } */

typedef struct objc_object { } *id ;
int x = 41 ;
extern id objc_msgSend(id self, char * op, ...);
extern int bogonic (int, int, int) ;
@interface Document {}
- (Document *) window;
- (Document *) class;
- (Document *) close;
@end
@implementation Document
- (Document *) class { return (Document *)0; }
- (Document *) close { return (Document *)0; }
- (Document *) window { return (Document *)0; }
- (void)willEndCloseSheet:(void *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo {
  [[self window] close];
  ((void (*)(id, char *, int))objc_msgSend)([self class], (char *)contextInfo, 1);
  ((void (*)(id, char *, int))bogonic)([self class], (char *)contextInfo, 1);
  bogonic (3, 4, 5);
  x++;
}
@end

/* Any symbol_ref of an un-stubified objc_msgSend is an error; look
   for "objc_msgSend" in quotes, without the $stub suffix. */
/* { dg-final {  scan-rtl-dump-not "symbol_ref.*\"objc_msgSend\"" "jump" } } */
