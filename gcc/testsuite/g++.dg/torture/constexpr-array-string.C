// Check character arrays produced by constant evaluation, including copies,
// mutation, embedded nuls, and implicit zero initialization.
// { dg-do run { target c++14 } }

struct Buffer { char data[3]; };
constexpr auto original = Buffer{{'a', 'b', 'c'}};

constexpr Buffer make_buffer ()
{
  Buffer b = original;
  b.data[1] = 'x';
  return b;
}

constexpr auto changed = make_buffer ();
constexpr auto embedded = Buffer{{'a', '\0', 'c'}};
constexpr auto padded = Buffer{{'a'}};

template <char C> struct Holder
{
  static constexpr Buffer value = Buffer{{'a', C, 'c'}};
};
template <char C> constexpr Buffer Holder<C>::value;

static_assert (original.data[1] == 'b', "");
static_assert (changed.data[1] == 'x', "");
static_assert (embedded.data[2] == 'c', "");
static_assert (padded.data[2] == '\0', "");
static_assert (Holder<'y'>::value.data[1] == 'y', "");
static_assert (Holder<'z'>::value.data[1] == 'z', "");

void check (const char *data, int precision, const char *expected, int length)
{
  char out[4];
  if (__builtin_snprintf (out, sizeof out, "%.*s", precision, data) != length
      || __builtin_memcmp (out, expected, length + 1))
    __builtin_abort ();
}

int main ()
{
  check (original.data + 1, 2, "bc", 2);
  check (changed.data + 1, 2, "xc", 2);
  check (embedded.data, 3, "a", 1);
  check (embedded.data + 1, 2, "", 0);
  check (embedded.data + 2, 1, "c", 1);
  check (padded.data, 3, "a", 1);
  check (padded.data + 1, 2, "", 0);
  check (Holder<'y'>::value.data + 1, 2, "yc", 2);
  check (Holder<'z'>::value.data + 1, 2, "zc", 2);

  Buffer copy = original;
  copy.data[1] = 'z';
  check (copy.data + 1, 2, "zc", 2);
  check (original.data + 1, 2, "bc", 2);
}
