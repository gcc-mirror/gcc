// { dg-do compile }
// { dg-options "-fcf-protection" }
static assert(__traits(getTargetInfo, "CET") != 0);
