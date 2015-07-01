// PR c++/65945
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=9" }

static_assert(alignof (decltype (nullptr)) == alignof (void *), "");
