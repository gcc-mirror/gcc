// { dg-do compile }
// { dg-options "-fcf-protection" }
static assert(__traits(getTargetInfo, "CET") == 3);
version (GNU_CET) {} else static assert(0);
