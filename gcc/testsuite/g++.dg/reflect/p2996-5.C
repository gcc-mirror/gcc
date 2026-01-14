// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [basic.fundamental].

#include <meta>

int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
void fn();
enum Enum { A };
using Alias = int;
struct B {};
struct S : B {
  int mem;
  int : 0;
};
template <auto> struct TCls {};
template <auto> void TFn();
template <auto> int TVar;
template <auto> concept Concept = requires { true; };
namespace NS {};
namespace NSAlias = NS;

constexpr auto r1 = std::meta::reflect_constant(42);  // represents int value of 42

constexpr auto r2 = std::meta::reflect_object(arr[1]);  // represents int object

constexpr auto r3 = ^^arr;      // represents a variable
constexpr auto r4 = ^^a3;       // represents a structured binding
constexpr auto r5 = ^^fn;       // represents a function
constexpr auto r6 = ^^Enum::A;  // represents an enumerator
constexpr auto r7 = ^^Alias;    // represents a type alias
constexpr auto r8 = ^^S;        // represents a type
constexpr auto r9 = ^^S::mem;   // represents a class member

constexpr auto r10 = std::meta::members_of (^^S, std::meta::access_context::current ())[1];
    // represents an unnamed bit-field

constexpr auto r11 = ^^TCls;     // represents a class template
constexpr auto r12 = ^^TFn;      // represents a function template
constexpr auto r13 = ^^TVar;     // represents a variable template
constexpr auto r14 = ^^Concept;  // represents a concept
constexpr auto r15 = ^^NSAlias;  // represents a namespace alias
constexpr auto r16 = ^^NS;       // represents a namespace

constexpr auto r17 = std::meta::bases_of(^^S, std::meta::access_context::current ())[0];
    // represents a direct base class relationship

constexpr auto r18 = std::meta::data_member_spec(^^int, {.name="member"});
    // represents a data member description
