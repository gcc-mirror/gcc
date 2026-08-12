// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype (^^::);
static_assert (sizeof (info) == sizeof (void *));
static_assert (alignof (info) == alignof (void *));
