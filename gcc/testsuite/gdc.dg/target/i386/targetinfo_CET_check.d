// { dg-do compile }
// { dg-options "-fcf-protection=check" }
static assert(__traits(getTargetInfo, "CET") == 0);
version (GNU_CET) static assert(0);
