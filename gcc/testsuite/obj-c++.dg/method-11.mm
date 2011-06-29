/* Check if class references (generated for the NeXT runtime) are appropriately 
   folded.  This test is safe to run on all targets.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

typedef TestsuiteObject ObjectTypedef1;
typedef ObjectTypedef1 ObjectTypedef2;
@compatibility_alias ObjectAlias1 ObjectTypedef2;
@compatibility_alias ObjectAlias2 ObjectAlias1;
typedef ObjectAlias2 ObjectTypedef3;

void foo(void) {
  id obj = [TestsuiteObject new];
  obj = [ObjectTypedef1 new];
  obj = [ObjectTypedef2 new];
  obj = [ObjectTypedef3 new];
  obj = [ObjectAlias1 new];
  obj = [ObjectAlias2 new];
}

/* { dg-final { scan-assembler "_OBJC_ClassRefs_0"  { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler "_OBJC_ClassRef_TestsuiteObject"  { target { *-*-darwin* && { lp64 } } } } } */
/* { dg-final { scan-assembler-not "_OBJC_ClassRefs_1" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler-not "_OBJC_ClassRef_ObjectTypedef" { target { *-*-darwin* && { lp64 } } } } } */
/* { dg-final { scan-assembler-not "_OBJC_ClassRef_ObjectAlias" { target { *-*-darwin* && { lp64 } } } } } */
