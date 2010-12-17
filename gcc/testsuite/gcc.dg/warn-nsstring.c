/* Check that the NSString format extension is rejected in c.  */
/* { dg-do compile } */

extern void NSLog (void *fmt, ...) __attribute__((format(__NSString__, 1, 2))); /* { dg-warning "is only allowed in Objective-C dialects" } */
extern void NSLog1 (void *fmt, ...) __attribute__((format(NSString, 1, 2))); /* { dg-warning "is only allowed in Objective-C dialects" } */


