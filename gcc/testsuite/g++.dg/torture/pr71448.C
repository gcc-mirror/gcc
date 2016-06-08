// PR c++/71448
// { dg-do compile }
// { dg-additional-options "-std=c++11" }

static constexpr const char foo[] = "foo";
static constexpr const char *bar = "bar";

static_assert ((foo + 3 - foo) == 3, "check");
static_assert (foo + 2 != foo, "check");
static_assert (foo + 2 >= foo, "check");
static_assert (3 + foo >= foo, "check");
static_assert (foo <= foo + 2, "check");
static_assert (foo <= 3 + foo, "check");
static_assert (foo + 2 > foo, "check");
static_assert (3 + foo > foo, "check");
static_assert (foo < 2 + foo, "check");
static_assert (foo < foo + 3, "check");
static_assert ((bar + 3 - bar) == 3, "check");
static_assert (bar + 2 != bar, "check");
static_assert (2 + bar >= bar, "check");
static_assert (bar + 3 >= bar, "check");
static_assert (bar <= bar + 2, "check");
static_assert (bar <= 3 + bar, "check");
static_assert (bar + 2 > bar, "check");
static_assert (3 + bar > bar, "check");
static_assert (bar < 2 + bar, "check");
static_assert (bar < bar + 3, "check");
