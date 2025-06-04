// { dg-do compile { target c++11 } }

template <typename... Ts>
void f (Ts&&... args)
{
  asm volatile ("" :: "r"(args) : "memory"); // { dg-error "parameter packs" }
}

void g()
{
  f(1);
}
