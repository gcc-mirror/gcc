// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on reflection-names.
//   reflection-name:
//    nested-name-specifier[opt] identifier
//    nested-name-specifier template identifier

int i;
using T = int;
typedef int TY;

template<typename T>
concept F = requires { typename T::type; };

template<typename T>
void foo (T) { }

void bar () { }

template<typename T>
T V{};

enum E { E1 };
struct S {
  static constexpr int i = 42;
  using type = int;
};
template<typename T>
struct C {
  static constexpr T t{};
  using type = T;
};

using US = S;
using UC = C<int>;

template<typename T>
using AT = C<T>;

namespace N {
  int n;
  using W = int;

  template<typename T>
  concept FF = requires { typename T::type; };

  namespace M {
    int m;
    using WW = int;
    enum EE { EE1 };
    struct SS {
      static constexpr int i = 42;
      using type = int;
    };
    template<typename T>
    struct CC {
      static constexpr T t{};
      using type = T;
    };

    using NMSS = SS;
    using NMCC = CC<int>;
  }
};

namespace NN = N;

// nested-name-specifier[opt] identifier
void
f1 (S s)
{
  constexpr auto m0 = ^^::N::M;
  constexpr auto m1 = ^^N::M;
  constexpr auto m2 = ^^N;
  constexpr auto m3 = ^^NN;
  constexpr auto m4 = ^^::N;
  constexpr auto m5 = ^^::NN;

  [: m0 :]::m = 0;
  [: m0 :]::WW v37 = 0;
  [: m1 :]::m = 0;
  [: m1 :]::WW v32 = 0;
  [: m2 :]::n = 0;
  [: m2 :]::W v33 = 0;
  [: m3 :]::n = 0;
  [: m3 :]::W v34 = 0;
  [: m4 :]::n = 0;
  [: m4 :]::W v35 = 0;
  [: m5 :]::n = 0;
  [: m5 :]::W v36 = 0;

  constexpr auto r1 = ^^i;
  constexpr auto r2 = ^^::i;
  constexpr auto r3 = ^^T;
  constexpr auto r4 = ^^::T;
  constexpr auto r5 = ^^::TY;
  constexpr auto r6 = ^^E::E1;
  constexpr auto r7 = ^^S::i;
  constexpr auto r8 = ^^S::type;
  constexpr auto r9 = ^^C<int>::t;
  constexpr auto r10 = ^^C<int>::type;
  constexpr auto r11 = ^^US::i;
  constexpr auto r12 = ^^US::type;
  constexpr auto r13 = ^^UC::t;
  constexpr auto r14 = ^^UC::type;
  constexpr auto r15 = ^^N::n;
  constexpr auto r16 = ^^N::W;
  constexpr auto r17 = ^^NN::n;
  constexpr auto r18 = ^^NN::W;
  constexpr auto r19 = ^^decltype(s)::i;
  constexpr auto r20 = ^^decltype(s)::type;
  // For pack-indexing, see pack-index1.C.
  constexpr auto rs = ^^S;
  constexpr auto r21 = ^^[: rs :]::type;
  constexpr auto r22 = ^^typename [: rs :]::type;
  constexpr auto rc = ^^C;
  constexpr auto r23 = ^^[: rc :]<int>::type;  // { dg-error "missing .template.|declared" }
  constexpr auto r24 = ^^typename [: rc :]<int>::type;
  constexpr auto r25 = ^^::N::n;
  constexpr auto r26 = ^^::N::W;
  constexpr auto r27 = ^^N::M::m;
  constexpr auto r28 = ^^N::M::WW;
  constexpr auto r29 = ^^N::M::EE;
  constexpr auto r30 = ^^N::M::EE::EE1;
  constexpr auto r31 = ^^[: m1 :]::EE::EE1;
  constexpr N::M::SS ss{};
  constexpr auto r32 = ^^decltype(ss)::type;
  constexpr N::M::CC<int> cc{};
  constexpr auto r33 = ^^decltype(cc)::type;
  constexpr auto r34 = ^^N::M::SS::type;
  constexpr auto r35 = ^^N::M::CC<int>::type;
  constexpr auto r36 = ^^N::M::NMSS::type;
  constexpr auto r37 = ^^N::M::NMCC::type;
  constexpr auto r38 = ^^[: m1 :]::SS::type;
  constexpr auto r39 = ^^typename [: m1 :]::SS::type;
  constexpr auto r40 = ^^[: m1 :]::CC<int>::type;
  constexpr auto r41 = ^^typename [: m1 :]::CC<int>::type;
  constexpr auto r42 = ^^::C<int>::type;

  [: r1 :] = 42;
  ++[: r2 :];
  typename [: r3 :] v1 = 3;
  typename [: r4 :] v2 = 2;
  typename [: r5 :] v3 = 1;
  int n = [: r6 :];
  n += [: r7 :];
  typename [: r8 :] v5 = 1;
  n += [: r9 :];
  typename [: r10 :] v6 = 4;
  n += [: r11 :];
  typename [: r12 :] v7 = 4;
  n += [: r13 :];
  typename [: r14 :] v8 = 4;
  [: r15 :]++;
  typename [: r16 :] v9 = 4;
  --[: r17 :];
  typename [: r18 :] v10 = 4;
  n *= [: r19 :];
  typename [: r20 :] v11 = 4;
  typename [: r21 :] v12 = 4;
  typename [: r22 :] v13 = 4;
  typename [: r24 :] v15 = 4;
  [: r25 :]--;
  typename [: r26 :] v16 = 7;
  [: r27 :] = 1;
  typename [: r28 :] v17 = 7;
  typename [: r29 :] v18;
  v18 = [: r30 :];
  v18 = [: r31 :];
  typename [: r32 :] v19 = 7;
  typename [: r33 :] v20 = 7;
  typename [: r34 :] v21 = 7;
  typename [: r35 :] v22 = 7;
  typename [: r36 :] v23 = 7;
  typename [: r37 :] v24 = 7;
  typename [: r38 :] v25 = 7;
  typename [: r39 :] v26 = 7;
  typename [: r40 :] v27 = 7;
  typename [: r41 :] v28 = 7;
  typename [: r42 :] v29 = 7;

  constexpr auto r46 = ^^::F;
  constexpr auto r47 = ^^::F;
  constexpr auto r48 = ^^N::FF;
  constexpr auto r49 = ^^::N::FF;
  constexpr auto r50 = ^^NN::FF;
  constexpr auto r51 = ^^::NN::FF;

  constexpr auto r52 = ^^C;
  constexpr auto r53 = ^^C<int>;
  constexpr auto r54 = ^^::C;
  constexpr auto r55 = ^^::template C;
  constexpr auto r56 = ^^::C<int>;
  constexpr auto r57 = ^^::template C<int>;

  typename [: r53 :] c1;
  typename [: r56 :] c2;
  typename [: r57 :] c3;

  constexpr auto r58 = ^^foo;
  constexpr auto r59 = ^^::foo;

  constexpr auto r60 = ^^V;
  constexpr auto r61 = ^^::V;

  constexpr auto r62 = ^^AT;
  constexpr auto r63 = ^^::AT;

  constexpr auto r64 = ^^bar;
  constexpr auto r65 = ^^::bar;
}

// nested-name-specifier template identifier
consteval void
f2 ()
{
  constexpr auto r1 = ^^::template C<int>;
  constexpr auto r2 = ^^::template C<int>::type;
  constexpr auto r3 = ^^N::M::template CC<int>::type;
  constexpr auto r4 = ^^N::M::template CC<int>::t;
  constexpr auto r5 = ^^N::M::template CC;
  constexpr auto r6 = ^^NN::M::template CC<int>::type;
  constexpr auto r7 = ^^NN::M::template CC<int>::t;
  constexpr auto r8 = ^^NN::M::template CC;

  // clang rejects these.
  constexpr auto r9 = ^^N::M::template NMCC::type;
  constexpr auto r10 = ^^N::M::template NMCC::t;
  constexpr auto r11 = ^^N::M::template NMCC;
  constexpr auto r12 = ^^NN::M::template NMCC::type;
  constexpr auto r13 = ^^NN::M::template NMCC::t;
  constexpr auto r14 = ^^NN::M::template NMCC;
  //constexpr auto r15 = ^^::template UC;
  constexpr auto r16 = ^^::template UC::type;

  typename [: r2 :] v1 = 7;
  typename [: r3 :] v2 = 7;
  int n = [: r4 :];
  typename [: r6 :] v3 = 1;
  n += [: r7 :];
}

template<typename T>
void
f3 (T t)
{
  /* Otherwise, if the identifier names a type alias that was introduced
     by the declaration of a template parameter, R represents the underlying
     entity of that type alias.
     So the reflection here should reflect the targ (int), not T itself?  */
  constexpr auto r = ^^T;
  static_assert (r == ^^int);
}

struct B {
  unsigned int a:32;
  unsigned int:32;
};

void
f4 ()
{
  constexpr auto r = ^^B::a;
}

void
g ()
{
  f1 (S{});
  f2 ();
  f3 (42);
}
