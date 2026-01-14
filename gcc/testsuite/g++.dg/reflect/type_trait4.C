// { dg-do compile { target { c++26 && int128 } } }
// { dg-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], sign modifications.

#include <meta>
using namespace std::meta;

static_assert (make_signed (^^const signed __int128) == ^^const __int128);
static_assert (make_signed (^^volatile __int128 unsigned) == ^^volatile __int128);

static_assert (make_unsigned (^^const volatile signed __int128) == ^^const volatile unsigned __int128);
static_assert (make_unsigned (^^__int128 unsigned) == ^^unsigned __int128);
