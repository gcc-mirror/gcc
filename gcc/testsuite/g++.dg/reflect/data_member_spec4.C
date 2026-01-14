// { dg-do compile { target c++26 } }
// { dg-options "-freflection" }
// Test std::meta::data_member_spec.

#include <meta>

using namespace std::meta;

consteval bool
valid_data_member_spec (info r, data_member_options opts)
{
  try { data_member_spec (r, opts); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (valid_data_member_spec (^^int, { .name = u8"👷" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"👷.♀" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"⏰" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"🕐" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"☠" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"💀" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"✋" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"👊" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"✈" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"🚀" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"☹" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"😀" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"💩" }));
