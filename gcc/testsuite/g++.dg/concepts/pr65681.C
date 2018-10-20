// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C()
{
  return requires (T t) { t.mf(); };
}

template<typename T>
concept bool CA1()
{
  return C<typename T::ca1_type>();
}

template<typename T>
concept bool CA2()
{
  return CA1<T>() && requires () { typename T::ca2_type; };
}

template<typename T>
concept bool CA3()
{
  return CA2<T>() && requires () { typename T::ca3_type; };
}

template<typename T>
concept bool CB1()
{
  return requires () { typename T::cb1_type; };
}

template<typename T>
concept bool CB2()
{
  return CB1<T>() && requires () { typename T::cb2_type; };
}

template<typename T>
concept bool CB3()
{
  return CB2<T>() && requires () { typename T::cb3_type; };
}


struct MC { void mf(); };
static_assert(C<MC>(), "");


struct MA1 { using ca1_type = MC; };
struct MA2 : MA1 { using ca2_type = int; };
struct MA3 : MA2 { using ca3_type = int; };
static_assert(CA1<MA1>(), "");
static_assert(CA2<MA2>(), "");
static_assert(CA3<MA3>(), "");

struct MB1 { using cb1_type = int; };
struct MB2 : MB1 { using cb2_type = int; };
struct MB3 : MB2 { using cb3_type = int; };
static_assert(CB1<MB1>(), "");
static_assert(CB2<MB2>(), "");
static_assert(CB3<MB3>(), "");


template<typename T1, typename T2>
struct S;

template<CA1 T1, CB1 T2>
struct S<T1, T2> // Specialization #1
{
  static constexpr int value = 1;
};

template<CA1 T1, CB2 T2>
  requires !CA2<T1>()
struct S<T1, T2> // Specialization #2
{
  static constexpr int value = 2;
};

template<CA2 T1, CB3 T2>
  requires !CA3<T1>()
struct S<T1, T2> // Specialization #3
{
  static constexpr int value = 3;
};

S<MA1,MB1> s11;
S<MA1,MB2> s12;
S<MA1,MB3> s13;
S<MA2,MB1> s21;
S<MA2,MB2> s22;
S<MA2,MB3> s23;
S<MA3,MB1> s31;
S<MA3,MB2> s32;
S<MA3,MB3> s33;

static_assert(S<MA1,MB1>::value == 1, "");
static_assert(S<MA1,MB2>::value == 2, "");
static_assert(S<MA1,MB3>::value == 2, "");
static_assert(S<MA2,MB1>::value == 1, "");
static_assert(S<MA2,MB2>::value == 1, "");
static_assert(S<MA2,MB3>::value == 3, "");
static_assert(S<MA3,MB1>::value == 1, "");
static_assert(S<MA3,MB2>::value == 1, "");
static_assert(S<MA3,MB3>::value == 1, "");
