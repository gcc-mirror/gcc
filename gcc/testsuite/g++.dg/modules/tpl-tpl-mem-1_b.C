// { dg-additional-options -fmodules-ts }

module foo;

static_assert (sizeof (outer<int>::inner<char>::other::type) == 1);
static_assert (sizeof (outer<char>::inner<int>::other::type) == sizeof (int));
