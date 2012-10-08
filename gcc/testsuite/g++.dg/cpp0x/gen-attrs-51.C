// { dg-do compile { target c++11 } }

typedef char layout_type;
struct A
{
    layout_type member [[gnu::aligned (16)]];
};

static_assert (sizeof (A) == 16, "Alignment should be 16");
