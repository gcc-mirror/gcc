// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], type properties.

#include <meta>
#include <utility>
#include <array>
#include <tuple>
#include <queue>
#include <stack>

using namespace std::meta;

class ClassType { };
enum EnumType { e0 };
struct ThrowCopyConsClass { ThrowCopyConsClass (const ThrowCopyConsClass &) noexcept (false); };
struct DeletedCopyAssignClass { DeletedCopyAssignClass & operator= (const DeletedCopyAssignClass &) = delete; };
struct DeletedMoveAssignClass { DeletedMoveAssignClass &operator= (DeletedMoveAssignClass &&) = delete; };
struct PODType { int i; int j; };
union UnionType { };

namespace N1
{
  struct Empty {};
  enum class SE { e1 };
  struct Abstract { virtual ~Abstract () = 0; };
  struct Any { template <class T> Any (T &&) {} };
  struct nAny { template <class... T> nAny (T &&...) {} };
  struct DelCopy { DelCopy (const DelCopy &) = delete; };
  struct Nontrivial {
    Nontrivial ();
    Nontrivial (const Nontrivial &);
    Nontrivial &operator= (const Nontrivial &);
    ~Nontrivial ();
  };
  union NontrivialUnion { int i; Nontrivial n; };
}

namespace funny1 {
  struct F {};
  void swap (F &, F &) = delete;
  void swap (F (&) [5], F (&) [5]);
  struct F2 { friend void swap (F2 &, F2 &) = delete; };
  struct F3 { friend void swap (F3 &, F3) {} };
  struct DummyCmp { template <class T> bool operator () (const T &, const T &) const { return false; } };
}

namespace funny2 {
  struct T0 {};
  void swap (T0, T0);
  struct T1 {};
  struct T2 {};
  void swap (T1, T2);
  void swap (T2, T1);
  struct BoolLike {};
  void swap (BoolLike, bool &);
  void swap (bool &, BoolLike);
  struct F0 {};
  void swap (F0, F0) = delete;
  struct F1 {};
  void swap (F0, F1) = delete;
  void swap (F1, F0) = delete;
  struct F2 {};
  struct F3 {};
  void swap (F2, F3);
  void swap (F3, F2) = delete;
  struct F4 { friend void swap (F4, F4) = delete; };
}

namespace funny3 {
  struct F {};
  void swap (F &, F &) = delete;
  void swap (F (&) [5], F (&) [5]) noexcept;
  void swap (F (&) [6], F (&) [6]);
  struct A {};
  void swap (A &, A &) noexcept (false);
}
namespace std {
  template <>
  void swap <funny3::A> (funny3::A &, funny3::A &) noexcept {}
  template <>
  void swap <funny3::A> (funny3::A (&) [3], funny3::A (&) [3]) noexcept (false) {}
}
namespace ns1 {
  struct SwapThrow {};
  void swap (SwapThrow &, SwapThrow &);
  void swap (SwapThrow (&) [3], SwapThrow (&) [3]) noexcept;
}
namespace ns2 {
  struct SwapThrow {
    SwapThrow () noexcept = default;
    SwapThrow (const SwapThrow &) noexcept (false);
    SwapThrow &operator = (const SwapThrow &) noexcept (false);
  };
}
namespace ns3 {
  struct SwapNoThrow {
    SwapNoThrow () noexcept = default;
    SwapNoThrow (const SwapNoThrow &) noexcept (false);
    SwapNoThrow &operator = (const SwapNoThrow &) noexcept (false);
  };
  void swap (SwapNoThrow &, SwapNoThrow &) noexcept;
}
namespace ns4 {
  struct SwapNoThrow {};
}
namespace ns5 {
  struct SwapThrow {
    SwapThrow () noexcept = default;
    SwapThrow (SwapThrow &&) noexcept;
    SwapThrow & operator = (const SwapThrow &) noexcept (false);
  };
}
namespace comps {
  struct CompareNoThrowCopyable {
    template <class T>
    bool operator () (const T &, const T &) const { return false; }
  };
  struct CompareNonCopyable {
    CompareNonCopyable () = default;
    CompareNonCopyable (const CompareNonCopyable &) = delete;
    CompareNonCopyable &operator= (const CompareNonCopyable &) noexcept;
    template <class T>
    bool operator () (const T &, const T &) const { return false; }
  };
  struct CompareThrowCopyable {
    CompareThrowCopyable () = default;
    CompareThrowCopyable (const CompareThrowCopyable &) noexcept (false);
    CompareThrowCopyable &operator = (const CompareThrowCopyable &);
    template <class T>
    bool operator () (const T &, const T &) const { return false; }
  };
}

namespace funny4 {
  struct T0 {};
  void swap (T0, T0) noexcept;
  struct T1 { friend void swap (T1, T1) noexcept; };
  struct T2 {};
  struct T3 {};
  void swap (T2, T3) noexcept;
  void swap (T3, T2) noexcept;
  struct T4 { operator T0 () const noexcept; };
  struct F0 {};
  void swap (F0, F0) = delete;
  struct F1 {};
  void swap (F1, F1);
  struct F2 {};
  void swap (F0, F2) noexcept;
  void swap (F2, F0);
  struct F3 { friend void swap (F3, F3) = delete; };
  struct F4 { friend void swap (F4, F4); };
  struct F5 { operator T0 () const; };
  struct BoolLike {};
  void swap (BoolLike, bool &) noexcept;
  void swap (bool &, BoolLike) noexcept;
  struct BoolLikeErr {};
  void swap (BoolLikeErr, bool &);
  void swap (bool &, BoolLikeErr) noexcept;
}

static_assert (is_swappable_type (^^int));
static_assert (is_swappable_type (^^bool));
static_assert (is_swappable_type (^^decltype (nullptr)));
static_assert (is_swappable_type (^^int &));
static_assert (is_swappable_type (^^int &&));
static_assert (is_swappable_type (^^int [1]));
static_assert (is_swappable_type (^^int [1][2]));
static_assert (is_swappable_type (^^int [1][2][3]));
static_assert (is_swappable_type (^^int (&) [1]));
static_assert (is_swappable_type (^^funny1::F [5]));
static_assert (is_swappable_type (^^funny1::F3));
static_assert (is_swappable_type (^^funny1::F3 [1]));
static_assert (is_swappable_type (^^funny1::F3 [1][2]));
static_assert (is_swappable_type (^^funny1::F3 [1][2][3]));
static_assert (is_swappable_type (^^ThrowCopyConsClass));
static_assert (is_swappable_type (^^EnumType));
static_assert (is_swappable_type (^^PODType));
static_assert (is_swappable_type (^^UnionType));
static_assert (is_swappable_type (^^N1::SE));
static_assert (is_swappable_type (^^N1::Empty));
static_assert (is_swappable_type (^^void *));
static_assert (is_swappable_type (^^int const *));
static_assert (is_swappable_type (^^ClassType *));
static_assert (is_swappable_type (^^int ClassType::*));
static_assert (is_swappable_type (^^void (ClassType::*) ()));
static_assert (is_swappable_type (^^N1::Nontrivial));
static_assert (is_swappable_type (^^N1::Any));
static_assert (is_swappable_type (^^N1::nAny));
static_assert (is_swappable_type (^^std::pair <int, int>));
static_assert (is_swappable_type (^^std::pair <int, int> [1]));
static_assert (is_swappable_type (^^std::pair <int, int> [1][2]));
static_assert (is_swappable_type (^^std::pair <int, int> [1][2][3]));
static_assert (is_swappable_type (^^std::pair <N1::Nontrivial, N1::Nontrivial>));
static_assert (is_swappable_type (^^std::tuple <int>));
static_assert (is_swappable_type (^^std::tuple <int> [1]));
static_assert (is_swappable_type (^^std::tuple <int> [1][2]));
static_assert (is_swappable_type (^^std::tuple <int> [1][2][3]));
static_assert (is_swappable_type (^^std::tuple <>));
static_assert (is_swappable_type (^^std::tuple <> [1]));
static_assert (is_swappable_type (^^std::tuple <> [1][2]));
static_assert (is_swappable_type (^^std::tuple <> [1][2][3]));
static_assert (is_swappable_type (^^std::tuple <N1::Nontrivial>));
static_assert (is_swappable_type (^^std::array <int, 1>));
static_assert (is_swappable_type (^^std::array <int, 1> [1]));
static_assert (is_swappable_type (^^std::array <int, 1> [1][2]));
static_assert (is_swappable_type (^^std::array <int, 1> [1][2][3]));
static_assert (is_swappable_type (^^std::array <N1::Nontrivial, 1>));
static_assert (is_swappable_type (^^std::array <int, 0>));
static_assert (is_swappable_type (^^std::array <N1::DelCopy, 0>));
static_assert (is_swappable_type (^^std::queue <int>));
static_assert (is_swappable_type (^^std::queue <int> [1]));
static_assert (is_swappable_type (^^std::queue <int> [1][2]));
static_assert (is_swappable_type (^^std::queue <int> [1][2][3]));
static_assert (is_swappable_type (^^std::queue <N1::Nontrivial>));
//static_assert (is_swappable_type (^^std::priority_queue <int>));
//static_assert (is_swappable_type (^^std::priority_queue <int> [1]));
//static_assert (is_swappable_type (^^std::priority_queue <int> [1][2]));
//static_assert (is_swappable_type (^^std::priority_queue <int> [1][2][3]));
//static_assert (is_swappable_type (^^std::priority_queue <N1::Nontrivial, std::vector <N1::Nontrivial>, funny1::DummyCmp>));
static_assert (is_swappable_type (^^std::stack <int>));
static_assert (is_swappable_type (^^std::stack <int> [1]));
static_assert (is_swappable_type (^^std::stack <int> [1][2]));
static_assert (is_swappable_type (^^std::stack <int> [1][2][3]));
static_assert (is_swappable_type (^^std::stack <N1::Nontrivial>));
static_assert (!is_swappable_type (^^void));
static_assert (!is_swappable_type (^^const void));
static_assert (!is_swappable_type (^^void ()));
static_assert (!is_swappable_type (^^void () const));
static_assert (!is_swappable_type (^^void () volatile));
static_assert (!is_swappable_type (^^void () const volatile));
static_assert (!is_swappable_type (^^const int));
static_assert (!is_swappable_type (^^const bool));
static_assert (!is_swappable_type (^^int []));
static_assert (!is_swappable_type (^^const int []));
static_assert (!is_swappable_type (^^int [][1]));
static_assert (!is_swappable_type (^^const int [1]));
static_assert (!is_swappable_type (^^const int [1][2]));
static_assert (!is_swappable_type (^^const int [1][2][3]));
static_assert (!is_swappable_type (^^N1::DelCopy));
static_assert (!is_swappable_type (^^N1::Abstract));
static_assert (!is_swappable_type (^^N1::NontrivialUnion));
static_assert (!is_swappable_type (^^funny1::F));
static_assert (!is_swappable_type (^^funny1::F [1]));
static_assert (!is_swappable_type (^^funny1::F [1][2]));
static_assert (!is_swappable_type (^^funny1::F [1][2][3]));
static_assert (!is_swappable_type (^^funny1::F [4]));
static_assert (!is_swappable_type (^^N1::DelCopy));
static_assert (!is_swappable_type (^^DeletedCopyAssignClass));
static_assert (!is_swappable_type (^^DeletedMoveAssignClass));
static_assert (!is_swappable_type (^^funny1::F2));
static_assert (!is_swappable_type (^^funny1::F2 [1]));
static_assert (!is_swappable_type (^^funny1::F2 [1][2]));
static_assert (!is_swappable_type (^^funny1::F2 [1][2][3]));
static_assert (!is_swappable_type (^^funny1::F2 [4]));
static_assert (!is_swappable_type (^^funny1::F2 [5]));

static_assert (is_swappable_with_type (^^int &, ^^int &));
static_assert (is_swappable_with_type (^^funny2::T0, ^^funny2::T0));
static_assert (is_swappable_with_type (^^funny2::T0, ^^const funny2::T0));
static_assert (is_swappable_with_type (^^funny2::T1, ^^funny2::T2));
static_assert (is_swappable_with_type (^^funny2::T2, ^^funny2::T1));
static_assert (is_swappable_with_type (^^funny2::BoolLike, ^^bool &));
static_assert (is_swappable_with_type (^^const funny2::BoolLike, ^^bool &));
static_assert (!is_swappable_with_type (^^int, ^^int));
static_assert (!is_swappable_with_type (^^int &, ^^unsigned &));
static_assert (!is_swappable_with_type (^^const int &, ^^const int &));
static_assert (!is_swappable_with_type (^^funny2::F0, ^^funny2::F0));
static_assert (!is_swappable_with_type (^^funny2::F0, ^^const funny2::F0));
static_assert (!is_swappable_with_type (^^funny2::T0, ^^funny2::T1));
static_assert (!is_swappable_with_type (^^funny2::F0, ^^funny2::F1));
static_assert (!is_swappable_with_type (^^funny2::F0, ^^const funny2::F1));
static_assert (!is_swappable_with_type (^^const funny2::F0, ^^funny2::F1));
static_assert (!is_swappable_with_type (^^funny2::F2, ^^funny2::F3));
static_assert (!is_swappable_with_type (^^funny2::F2, ^^const funny2::F3));
static_assert (!is_swappable_with_type (^^const funny2::F2, ^^funny2::F3));
static_assert (!is_swappable_with_type (^^funny2::F4, ^^funny2::F4));
static_assert (!is_swappable_with_type (^^funny2::BoolLike, ^^funny2::BoolLike));

static_assert (is_nothrow_swappable_type (^^int));
static_assert (is_nothrow_swappable_type (^^bool));
static_assert (is_nothrow_swappable_type (^^decltype (nullptr)));
static_assert (is_nothrow_swappable_type (^^int &));
static_assert (is_nothrow_swappable_type (^^int &&));
static_assert (is_nothrow_swappable_type (^^int [1]));
static_assert (is_nothrow_swappable_type (^^int [1][2]));
static_assert (is_nothrow_swappable_type (^^int [1][2][3]));
static_assert (is_nothrow_swappable_type (^^funny3::F [5]));
static_assert (is_nothrow_swappable_type (^^EnumType));
static_assert (is_nothrow_swappable_type (^^PODType));
static_assert (is_nothrow_swappable_type (^^UnionType));
static_assert (is_nothrow_swappable_type (^^N1::SE));
static_assert (is_nothrow_swappable_type (^^N1::Empty));
static_assert (is_nothrow_swappable_type (^^void *));
static_assert (is_nothrow_swappable_type (^^void (*) ()));
static_assert (is_nothrow_swappable_type (^^int const *));
static_assert (is_nothrow_swappable_type (^^ClassType *));
static_assert (is_nothrow_swappable_type (^^int ClassType::*));
static_assert (is_nothrow_swappable_type (^^void (ClassType::*) ()));
static_assert (is_nothrow_swappable_type (^^int (ClassType::*) () const volatile));
static_assert (is_nothrow_swappable_type (^^ns1::SwapThrow [3]));
static_assert (is_nothrow_swappable_type (^^ns3::SwapNoThrow));
static_assert (is_nothrow_swappable_type (^^ns3::SwapNoThrow [1]));
static_assert (is_nothrow_swappable_type (^^ns3::SwapNoThrow [3]));
static_assert (is_nothrow_swappable_type (^^ns3::SwapNoThrow [2][3][4]));
static_assert (is_nothrow_swappable_type (^^ns4::SwapNoThrow));
static_assert (is_nothrow_swappable_type (^^ns4::SwapNoThrow [1]));
static_assert (is_nothrow_swappable_type (^^ns4::SwapNoThrow [3]));
static_assert (is_nothrow_swappable_type (^^ns4::SwapNoThrow [2][3][4]));
static_assert (is_nothrow_swappable_type (^^std::pair <int, int>));
static_assert (is_nothrow_swappable_type (^^std::pair <int, int> [1]));
static_assert (is_nothrow_swappable_type (^^std::pair <int, int> [1][2]));
static_assert (is_nothrow_swappable_type (^^std::tuple <int>));
static_assert (is_nothrow_swappable_type (^^std::tuple <int> [1]));
static_assert (is_nothrow_swappable_type (^^std::tuple <int> [1][2]));
static_assert (is_nothrow_swappable_type (^^std::tuple <>));
static_assert (is_nothrow_swappable_type (^^std::tuple <> [1]));
static_assert (is_nothrow_swappable_type (^^std::tuple <> [1][2]));
static_assert (is_nothrow_swappable_type (^^std::array <int, 1>));
static_assert (is_nothrow_swappable_type (^^std::array <int, 0>));
static_assert (is_nothrow_swappable_type (^^std::array <N1::DelCopy, 0>));
static_assert (is_nothrow_swappable_type (^^std::array <ns1::SwapThrow, 0>));
static_assert (is_nothrow_swappable_type (^^std::queue <int>));
//static_assert (is_nothrow_swappable_type (^^std::priority_queue <int>));
static_assert (is_nothrow_swappable_type (^^std::stack <int>));
//static_assert (is_nothrow_swappable_type (^^std::priority_queue <int, std::vector <int>, comps::CompareNoThrowCopyable>));
static_assert (!is_nothrow_swappable_type (^^void));
static_assert (!is_nothrow_swappable_type (^^const void));
static_assert (!is_nothrow_swappable_type (^^void ()));
static_assert (!is_nothrow_swappable_type (^^void () const));
static_assert (!is_nothrow_swappable_type (^^void () volatile));
static_assert (!is_nothrow_swappable_type (^^void () const volatile));
static_assert (!is_nothrow_swappable_type (^^const int));
static_assert (!is_nothrow_swappable_type (^^const bool));
static_assert (!is_nothrow_swappable_type (^^const int [1]));
static_assert (!is_nothrow_swappable_type (^^const int [1][2]));
static_assert (!is_nothrow_swappable_type (^^const int [1][2][3]));
static_assert (!is_nothrow_swappable_type (^^int []));
static_assert (!is_nothrow_swappable_type (^^const int []));
static_assert (!is_nothrow_swappable_type (^^int [][1]));
static_assert (!is_nothrow_swappable_type (^^const funny3::F [5]));
static_assert (!is_nothrow_swappable_type (^^N1::Abstract));
static_assert (!is_nothrow_swappable_type (^^N1::DelCopy));
static_assert (!is_nothrow_swappable_type (^^funny3::F));
static_assert (!is_nothrow_swappable_type (^^funny3::F [1]));
static_assert (!is_nothrow_swappable_type (^^funny3::F [1][2]));
static_assert (!is_nothrow_swappable_type (^^funny3::F [1][2][3]));
static_assert (!is_nothrow_swappable_type (^^funny3::F [6]));
static_assert (!is_nothrow_swappable_type (^^funny3::A));
static_assert (!is_nothrow_swappable_type (^^funny3::A [3]));
static_assert (!is_nothrow_swappable_type (^^ns1::SwapThrow));
static_assert (!is_nothrow_swappable_type (^^ns1::SwapThrow [1]));
static_assert (!is_nothrow_swappable_type (^^ns1::SwapThrow [3][2]));
static_assert (!is_nothrow_swappable_type (^^ns1::SwapThrow [2][3][4]));
static_assert (!is_nothrow_swappable_type (^^ns2::SwapThrow));
static_assert (!is_nothrow_swappable_type (^^ns2::SwapThrow [1]));
static_assert (!is_nothrow_swappable_type (^^ns2::SwapThrow [2][3][4]));
static_assert (!is_nothrow_swappable_type (^^ns5::SwapThrow));
static_assert (!is_nothrow_swappable_type (^^ns5::SwapThrow [1]));
static_assert (!is_nothrow_swappable_type (^^ns5::SwapThrow [2][3][4]));
static_assert (!is_nothrow_swappable_type (^^ThrowCopyConsClass));
static_assert (!is_nothrow_swappable_type (^^std::pair <ThrowCopyConsClass, ThrowCopyConsClass>));
static_assert (!is_nothrow_swappable_type (^^std::tuple <ThrowCopyConsClass>));
static_assert (!is_nothrow_swappable_type (^^std::array <ThrowCopyConsClass, 1>));
static_assert (is_nothrow_swappable_type (^^std::queue <ThrowCopyConsClass>));
static_assert (is_nothrow_swappable_type (^^std::priority_queue <ThrowCopyConsClass, std::vector <ThrowCopyConsClass>, comps::CompareNoThrowCopyable>));
static_assert (is_nothrow_swappable_type (^^std::stack <ThrowCopyConsClass>));
static_assert (!is_nothrow_swappable_type (^^std::priority_queue <int, std::vector <int>, comps::CompareNonCopyable>));
static_assert (!is_nothrow_swappable_type (^^std::priority_queue <int, std::vector <int>, comps::CompareThrowCopyable>));

static_assert (is_nothrow_swappable_with_type (^^int &, ^^int &));
static_assert (is_nothrow_swappable_with_type (^^funny4::T0, ^^funny4::T0));
static_assert (is_nothrow_swappable_with_type (^^funny4::T0, ^^const funny4::T0));
static_assert (is_nothrow_swappable_with_type (^^funny4::T1, ^^funny4::T1));
static_assert (is_nothrow_swappable_with_type (^^funny4::T1, ^^const funny4::T1));
static_assert (is_nothrow_swappable_with_type (^^funny4::T2, ^^funny4::T3));
static_assert (is_nothrow_swappable_with_type (^^funny4::T3, ^^funny4::T2));
static_assert (is_nothrow_swappable_with_type (^^funny4::T0, ^^funny4::T4));
static_assert (is_nothrow_swappable_with_type (^^funny4::T4, ^^funny4::T0));
static_assert (is_nothrow_swappable_with_type (^^funny4::BoolLike, ^^bool &));
static_assert (is_nothrow_swappable_with_type (^^const funny4::BoolLike, ^^bool &));
static_assert (!is_nothrow_swappable_with_type (^^const int &, ^^const int &));
static_assert (!is_nothrow_swappable_with_type (^^int &, ^^unsigned &));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F0, ^^funny4::F0));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F0, ^^const funny4::F0));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F1, ^^funny4::F1));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F1, ^^const funny4::F1));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F0, ^^funny4::F2));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F2, ^^funny4::F0));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F3, ^^funny4::F3));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F3, ^^const funny4::F3));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F4, ^^funny4::F4));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F4, ^^const funny4::F4));
static_assert (!is_nothrow_swappable_with_type (^^funny4::T0, ^^funny4::F5));
static_assert (!is_nothrow_swappable_with_type (^^funny4::F5, ^^funny4::T0));
static_assert (!is_nothrow_swappable_with_type (^^funny4::BoolLikeErr, ^^bool &));
static_assert (!is_nothrow_swappable_with_type (^^const funny4::BoolLikeErr, ^^bool &));

static_assert (unwrap_reference (^^int) == ^^int);
static_assert (unwrap_reference (^^const int) == ^^const int);
static_assert (unwrap_reference (^^const int &) == ^^const int &);
static_assert (unwrap_reference (^^const int *) == ^^const int *);
static_assert (unwrap_reference (^^const int *&) == ^^const int *&);
static_assert (unwrap_reference (^^std::reference_wrapper <int>) == ^^int &);
static_assert (unwrap_reference (^^std::reference_wrapper <const int>) == ^^const int &);
static_assert (unwrap_reference (^^std::reference_wrapper <long>) == ^^long &);
static_assert (unwrap_reference (^^const std::reference_wrapper <int>) == ^^const std::reference_wrapper <int>);
static_assert (unwrap_reference (^^volatile std::reference_wrapper <int>) == ^^volatile std::reference_wrapper <int>);
static_assert (unwrap_reference (^^const volatile std::reference_wrapper <int>) == ^^const volatile std::reference_wrapper <int>);
static_assert (unwrap_reference (^^std::reference_wrapper <int> &) == ^^std::reference_wrapper <int> &);
static_assert (unwrap_reference (^^std::reference_wrapper <int> &&) == ^^std::reference_wrapper <int> &&);
static_assert (unwrap_reference (^^const std::reference_wrapper <int> &) == ^^const std::reference_wrapper <int> &);

static_assert (unwrap_ref_decay (^^int) == decay (^^int));
static_assert (unwrap_ref_decay (^^const int) == decay (^^const int));
static_assert (unwrap_ref_decay (^^const int &) == decay (^^const int &));
static_assert (unwrap_ref_decay (^^const int *) == decay (^^const int *));
static_assert (unwrap_ref_decay (^^const int *&) == decay (^^const int *&));
static_assert (unwrap_ref_decay (^^std::reference_wrapper <int>) == ^^int &);
static_assert (unwrap_ref_decay (^^std::reference_wrapper <int> &) == ^^int &);
static_assert (unwrap_ref_decay (^^const std::reference_wrapper <int>) == ^^int &);
static_assert (unwrap_ref_decay (^^const std::reference_wrapper <int> &) == ^^int &);
static_assert (unwrap_ref_decay (^^std::reference_wrapper <const int>) == ^^const int &);
static_assert (unwrap_ref_decay (^^std::reference_wrapper <const int> &) == ^^const int &);
static_assert (unwrap_ref_decay (^^std::reference_wrapper <long>) == ^^long &);
