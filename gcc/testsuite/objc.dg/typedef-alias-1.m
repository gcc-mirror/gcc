/* Typedefs of ObjC types should work without any bogus warnings. */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include <objc/objc.h>

typedef TestsuiteObject MyObject;

int main (int argc, const char * argv[])
{
    TestsuiteObject* a = nil;
    MyObject* b = a;
    TestsuiteObject* c = b;

    return 0;
}

