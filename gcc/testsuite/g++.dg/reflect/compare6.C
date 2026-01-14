// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

enum Enum { A, B, C };
enum class EnumCls { A, B, C };

constexpr info rB = ^^B, rClsB = ^^EnumCls::B;
static_assert(rB != rClsB);
static_assert(int([:rB:]) == int([:rClsB:]));
static_assert(static_cast<Enum>([:rClsB:]) == B);
