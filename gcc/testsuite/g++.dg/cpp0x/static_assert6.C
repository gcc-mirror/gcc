// PR c++/51397
// { dg-do compile { target c++11 } }

static_assert('X' != '\130', "'X' has the wrong value"); // { dg-error "'X' has the wrong value" }
