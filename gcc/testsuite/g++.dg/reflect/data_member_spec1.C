// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::data_member_spec.

#include <meta>

using namespace std::meta;

enum E { E0, E1 };
struct S {};

consteval bool
valid_data_member_spec (info r, data_member_options opts)
{
  try { data_member_spec (r, opts); }
  catch (std::meta::exception &) { return false; }
  return true;
}

consteval bool
foo ()
{
  std::string n1 = "bar";
  data_member_options a = { .name = n1 };
  auto dmsa = data_member_spec (^^S, a);
  if (!is_data_member_spec (dmsa))
    throw 1;
  std::u8string_view n2 = u8"baz";
  data_member_options b = { .name = n2, .alignment = 2 * alignof (long), .no_unique_address = true };
  auto dmsb = data_member_spec (^^long, b);
  if (!is_data_member_spec (dmsb))
    throw 2;
  data_member_options c = { .bit_width = 7 };
  auto dmsc = data_member_spec (^^int, c);
  if (!is_data_member_spec (dmsc))
    throw 3;
  data_member_options d = { .name = "extremely_long_identifier1", .bit_width = 6 };
  auto dmsd = data_member_spec (^^E, d);
  if (!is_data_member_spec (dmsd))
    throw 4;
  data_member_options e = { .bit_width = 0 };
  auto dmse = data_member_spec (^^int, e);
  if (!is_data_member_spec (dmse))
    throw 5;
  data_member_options f = { .name = u8"extremely_long_identifier2", .alignment = 2 * alignof (E) };
  auto dmsf = data_member_spec (^^E &, f);
  if (!is_data_member_spec (dmsf))
    throw 6;
  if (dmsa != data_member_spec (^^S, { .name = "bar" }))
    throw 7;
  if (dmsb != data_member_spec (^^long, { .name = u8"baz", .alignment = 2 * alignof (long), .no_unique_address = true }))
    throw 8;
  if (dmsc != data_member_spec (^^int, { .bit_width = 7ULL }))
    throw 9;
  if (dmsd != data_member_spec (^^E, { .name = "extremely_long_identifier1", .bit_width = 6 }))
    throw 10;
  if (dmse != data_member_spec (^^int, { .bit_width = 0U }))
    throw 11;
  if (dmsf != data_member_spec (^^E &, { .name = u8"extremely_long_identifier2", .alignment = 2 * alignof (E) }))
    throw 12;
  if (dmsa == data_member_spec (^^const S, { .name = "bar" }))
    throw 13;
  if (dmsa == data_member_spec (^^S, { .name = "foo" }))
    throw 14;
  if (dmsb == data_member_spec (^^long, { .name = u8"baz", .alignment = 4 * alignof (long), .no_unique_address = true }))
    throw 15;
  if (dmsb == data_member_spec (^^long, { .name = u8"baz", .alignment = 2 * alignof (long), .no_unique_address = false }))
    throw 16;
  if (dmsc == data_member_spec (^^int, { .bit_width = 8 }))
    throw 17;
  if (dmsd == data_member_spec (^^E, { .name = "extremely_long_identifier3", .bit_width = 6 }))
    throw 18;
  if (dmsd == data_member_spec (^^E, { .name = "extremely_long_identifier", .bit_width = 5 }))
    throw 19;
  if (dmse == data_member_spec (^^int, { .name = "a" }))
    throw 20;
  if (data_member_spec (^^int, { .name = "a" }) == data_member_spec (^^int, { .name = "a", .no_unique_address = true }))
    throw 21;
  return true;
}

static_assert (foo ());

static_assert (!valid_data_member_spec (^^void, { .name = "a" }));
static_assert (!valid_data_member_spec (^^int (int), { .name = "a" }));
static_assert (!valid_data_member_spec (^^int, {}));
static_assert (!valid_data_member_spec (^^int, { .alignment = alignof (int) }));
static_assert (!valid_data_member_spec (^^int, { .no_unique_address = true }));
static_assert (!valid_data_member_spec (^^int, { .name = "for" }));
static_assert (!valid_data_member_spec (^^int, { .name = "if" }));
static_assert (!valid_data_member_spec (^^int, { .name = "char8_t" }));
static_assert (!valid_data_member_spec (^^int, { .name = "virtual" }));
static_assert (!valid_data_member_spec (^^int, { .name = "static_assert" }));
static_assert (!valid_data_member_spec (^^int, { .name = "__is_convertible" }));
static_assert (!valid_data_member_spec (^^int, { .name = "__builtin_is_implicit_lifetime" }));
static_assert (!valid_data_member_spec (^^int, { .name = "007" }));
static_assert (!valid_data_member_spec (^^int, { .name = "operator++" }));
static_assert (!valid_data_member_spec (^^int, { .name = "operator ++" }));
static_assert (!valid_data_member_spec (^^int, { .name = "foo\\u00AA" }));
static_assert (!valid_data_member_spec (^^int, { .name = "+ -" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"\u00AA" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"\u00AB" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"\u00B6" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"\u00BA" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"\u00C0" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"\u00D6" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"\u0384" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"\u0669" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"A\u0669" }));
static_assert (!valid_data_member_spec (^^int, { .name = u8"\u0E59" }));
static_assert (valid_data_member_spec (^^int, { .name = u8"A\u0E59" }));
static_assert (!valid_data_member_spec (^^S, { .name = "a", .bit_width = 4 }));
static_assert (!valid_data_member_spec (^^int, { .name = "a", .alignment = alignof (int), .bit_width = 4 }));
static_assert (!valid_data_member_spec (^^int, { .name = "a", .bit_width = 4, .no_unique_address = true }));
static_assert (!valid_data_member_spec (^^int, { .name = "a", .bit_width = 0 }));
static_assert (!valid_data_member_spec (^^int, { .name = "a", .bit_width = -42 }));
static_assert (!valid_data_member_spec (^^int, { .name = "a", .alignment = 41 }));
static_assert (!valid_data_member_spec (^^int, { .name = "a", .alignment = -__INT_MAX__ - 1 }));
static_assert (!valid_data_member_spec (^^long long, { .name = "a", .alignment = alignof (long long) / 2 }));
