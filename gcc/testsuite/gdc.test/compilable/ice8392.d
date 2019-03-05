// EXTRA_SOURCES: imports/a8392.d

module ice8392;

struct A
{
}

auto fooa(alias handler)(A a)
{
    return handler(null);
}
