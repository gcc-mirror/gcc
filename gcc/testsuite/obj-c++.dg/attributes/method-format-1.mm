/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

#include <objc/objc.h>
#include <stdlib.h>

@interface LogObject
{
  Class isa;
} 
+ (void) log: (int)level  message: (const char *) my_format, ...  __attribute__ ((format (printf, 2, 3)));
- (void) log: (int)level  message: (const char *) my_format, ...  __attribute__ ((format (printf, 2, 3)));

+ (void) debug: (const char *) my_format, ...  __attribute__ ((format (printf, 1, 2)));
- (void) debug: (const char *) my_format, ...  __attribute__ ((format (printf, 1, 2)));

/* Just make sure a missing or invalid attribute won't crash the compiler.  */
- (void) log2: (int)level  message: (const char *) my_format, ...  __attribute__ ((format (printf, 2)));    /* { dg-error "wrong" } */
+ (void) debug2: (const char *) my_format, ...  __attribute__ ((format (printf))); /* { dg-error "wrong" } */
- (void) debug2: (const char *) my_format, ...  __attribute__ ((format (printf))); /* { dg-error "wrong" } */
+ (void) alert: (const char *) my_format __attribute__ ((format (printf, 1, 2))); /* { dg-error "does not refer to a variable argument list" } */
- (void) alert: (const char *) my_format __attribute__ ((format (printf, 1, 2))); /* { dg-error "does not refer to a variable argument list" } */
@end

void test (LogObject *object)
{
  [object log: 2  message: "attribute only applies to variadic functions"];
  [object log: 2  message: "attribute %s only applies to variadic functions", "'format'"];
  [object log: 2  message: "attribute %s only applies to variadic functions"]; /* { dg-warning "expects a matching" } */

  [object debug: "attribute only applies to variadic functions"];
  [object debug: "attribute %s only applies to variadic functions", "'format'"];
  [object debug: "attribute %s only applies to variadic functions"]; /* { dg-warning "expects a matching" } */

  [LogObject log: 2  message: "attribute only applies to variadic functions"];
  [LogObject log: 2  message: "attribute %s only applies to variadic functions", "'format'"];
  [LogObject log: 2  message: "attribute %s only applies to variadic functions"]; /* { dg-warning "expects a matching" } */

  [LogObject debug: "attribute only applies to variadic functions"];
  [LogObject debug: "attribute %s only applies to variadic functions", "'format'"];
  [LogObject debug: "attribute %s only applies to variadic functions"]; /* { dg-warning "expects a matching" } */
}
