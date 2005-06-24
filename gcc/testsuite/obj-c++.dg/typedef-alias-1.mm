/* Typedefs of ObjC types should work without any bogus warnings. */
/* { dg-do compile } */

#include <objc/Object.h>

typedef Object MyObject;

int main (int argc, const char * argv[])
{
    Object* a = nil;
    MyObject* b = a;
    Object* c = b;

    return 0;
}

