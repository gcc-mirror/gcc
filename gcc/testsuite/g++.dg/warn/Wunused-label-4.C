// PR c++/113582
// { dg-do compile { target c++17 } }
// { dg-options "-Wunused-label" }

template<bool B> void
do_something ()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-label"
start:
  if constexpr(B)
    goto start;
#pragma GCC diagnostic pop
}

template<bool B> void
do_something2 ()
{
start: // { dg-warning "defined but not used" }
  if constexpr(B)
    goto start;
}

void
g ()
{
  do_something<0>();
  do_something2<0>();
}
