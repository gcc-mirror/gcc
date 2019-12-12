// PR c++/80864
// { dg-do compile { target c++11 } }
// { dg-options "-Wmissing-braces" }

struct S {
  char c[1];
};

template <typename T>
void
fn ()
{
   constexpr S s1 = S{};
   constexpr S s2 = S{{}};
   constexpr S s3 = S{{{}}};
   constexpr S s4 = {};
   constexpr S s5 = {{}};
   constexpr S s6 = {{{}}};
   constexpr S s7{{}};
   constexpr S s8{S{}};
   constexpr S s9{S{{}}};
   constexpr S s10{S{{{}}}};
   constexpr S s11 = S();
   constexpr S s12 = S({});
   constexpr S s13 = S({{}});
   constexpr S s14 = {{}};
   constexpr S s15 = {{{}}};
}

void
foo ()
{
  fn<int>();
}
