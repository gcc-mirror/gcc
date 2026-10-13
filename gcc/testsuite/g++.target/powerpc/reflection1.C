// { dg-do compile { target c++26 } }
// { dg-require-effective-target power10_ok }
// { dg-additional-options "-freflection -mcpu=power10" }

using info = decltype (^^::);

template <typename T> struct W {};

void f (W<info>) {}
void f (W<__vector_quad>) {}
void f (W<__vector_pair>) {}

static_assert (^^info != ^^__vector_quad);
static_assert (__is_scalar (info));
static_assert (!__is_same (info, __vector_quad));

// { dg-final { scan-assembler "_Z1f1WIDmE" } }
// { dg-final { scan-assembler "_Z1f1WIu13__vector_quadE" } }
// { dg-final { scan-assembler "_Z1f1WIu13__vector_pairE" } }
