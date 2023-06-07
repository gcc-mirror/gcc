// { dg-lto-do link }
// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" }

#include "20091026-1_a.h"
cObject *cHead::find(const char *objname) const
{
    return firstchildp;
}
class cNetworkType : public cObject { };
cNetworkType *networktype;

