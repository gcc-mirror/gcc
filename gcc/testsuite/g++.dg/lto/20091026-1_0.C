// { dg-lto-do link }
// { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" }

#include "20091026-1_a.h"
cObject *cHead::find(const char *objname) const
{
    return firstchildp;
}
class cNetworkType : public cObject { };
cNetworkType *networktype;

