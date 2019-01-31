// PR c++/80864
// { dg-do compile { target c++11 } }

struct S {
  int c[3];
};

template <typename T, int N>
void
fn ()
{
   constexpr S s1 = S{N};
   constexpr S s2 = S{{N, N}};
   constexpr S s3 = S{N, N};
   constexpr S s4 = {N};
   constexpr S s5 = {{N}};
   constexpr S s6 = {N, N};
   constexpr S s7{{N}};
   constexpr S s8{S{N}};
   constexpr S s9{S{{N}}};
   constexpr S s10{S{{N}}};
   constexpr S s11 = S({N});
   constexpr S s12 = S({{N}});
   constexpr S s13 = {{N}};
   constexpr S s14 = {{N, N, N}};
}

void
foo ()
{
  fn<int, 10>();
}
