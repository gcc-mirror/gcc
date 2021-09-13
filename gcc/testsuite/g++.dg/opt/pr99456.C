// PR c++/99456
// { dg-do compile { target c++17 } }
// { dg-options "-g0" }
// { dg-final { scan-assembler-not "PR99456Var0\[1234]" } }
// { dg-final { scan-assembler-not "__static_initialization_and_destruction" } }
// { dg-final { scan-assembler-not "_GLOBAL__sub_I" } }
// { dg-final { scan-assembler-not "_ZGV12PR99456Var1\[1234]" } }

typedef __UINTPTR_TYPE__ uintptr_t;

class Container
{
public:
  uintptr_t m;
};

extern unsigned desc;
static constexpr unsigned &descRef = desc;

inline Container PR99456Var01 {reinterpret_cast<uintptr_t> (&descRef)};
inline Container PR99456Var02 {reinterpret_cast<uintptr_t> (&desc)};
inline uintptr_t PR99456Var03 {reinterpret_cast<uintptr_t> (&descRef)};
inline uintptr_t PR99456Var04 {reinterpret_cast<uintptr_t> (&desc)};

inline Container PR99456Var11 {reinterpret_cast<uintptr_t> (&descRef)};
inline Container PR99456Var12 {reinterpret_cast<uintptr_t> (&desc)};
inline uintptr_t PR99456Var13 {reinterpret_cast<uintptr_t> (&descRef)};
inline uintptr_t PR99456Var14 {reinterpret_cast<uintptr_t> (&desc)};

auto *PR99456Ref11 = &PR99456Var11;
auto *PR99456Ref12 = &PR99456Var12;
auto *PR99456Ref13 = &PR99456Var13;
auto *PR99456Ref14 = &PR99456Var14;
