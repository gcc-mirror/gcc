// { dg-additional-options -fmodules-ts }
module m0;

static_assert (!m0_ns::s0<int>::a);
