// PR c++/51397
// { dg-options "-std=c++0x" }

static_assert('X' != '\130', "'X' has the wrong value"); // { dg-error "'X' has the wrong value" }
