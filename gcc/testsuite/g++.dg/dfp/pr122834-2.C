// PR c++/122834
// { dg-do compile { target { c++11 && float16 } } }
// { dg-options "" }
// { dg-add-options float16 }

typedef decltype (0.0DL) A;
typedef _Float16 B;
void bar (A);

void
foo (B x)
{
  bar (x);
}

auto a = 0.0DL + 1.0F16;
auto b = 1.0F16 + 0.0DL;
static_assert (__is_same_as (decltype (0.0DL + 1.0F16), decltype (0.0DL)));
static_assert (__is_same_as (decltype (1.0F16 + 0.0DL), decltype (0.0DL)));
