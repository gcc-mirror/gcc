// { dg-additional-options "-fmodules" }

module M;

static_assert(no_odr_use_cexpr() == 123);
static_assert(test_md() == nullptr);
static_assert(test_mfn() == nullptr);
static_assert(test_bitfield() == 4);
static_assert(A{}.value == 789);
static_assert(A{}.a() == 789);
