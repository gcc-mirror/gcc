// Tangentially to PR c++/80351
// { dg-do compile { target c++17 } }
// { dg-options "-Wunused-variable" }
#include <initializer_list>

// Warnings:
static int           int_s1  = 0; // { dg-warning "defined but not used" }
static int           int_s2  = 0; // { dg-warning "defined but not used" }
inline static int    int_is1 = 0; // { dg-warning "defined but not used" }
inline static int    int_is2 = 0; // { dg-warning "defined but not used" }
// No warnings:
constexpr static int int_cs1 = 0; // { dg-bogus "defined but not used" }
constexpr static int int_cs2 = 0; // { dg-bogus "defined but not used" }
int                  int_1   = 0; // { dg-bogus "defined but not used" }
int                  int_2   = 0; // { dg-bogus "defined but not used" }
inline int           int_i1  = 0; // { dg-bogus "defined but not used" }
inline int           int_i2  = 0; // { dg-bogus "defined but not used" }
constexpr int        int_c1  = 0; // { dg-bogus "defined but not used" }
constexpr int        int_c2  = 0; // { dg-bogus "defined but not used" }

// Warnings:
static auto           int_as1  = 0; // { dg-warning "defined but not used" }
static auto           int_as2  = 0; // { dg-warning "defined but not used" }
inline static auto    int_ais1 = 0; // { dg-warning "defined but not used" }
inline static auto    int_ais2 = 0; // { dg-warning "defined but not used" }
// No warnings:
constexpr static auto int_acs1 = 0; // { dg-bogus "defined but not used" }
constexpr static auto int_acs2 = 0; // { dg-bogus "defined but not used" }
auto                  int_a1   = 0; // { dg-bogus "defined but not used" }
auto                  int_a2   = 0; // { dg-bogus "defined but not used" }
inline auto           int_ai1  = 0; // { dg-bogus "defined but not used" }
inline auto           int_ai2  = 0; // { dg-bogus "defined but not used" }
constexpr auto        int_ac1  = 0; // { dg-bogus "defined but not used" }
constexpr auto        int_ac2  = 0; // { dg-bogus "defined but not used" }

// Warnings:
static std::initializer_list<int>           il_s1  = {0, 1}; // { dg-warning "defined but not used" }
static std::initializer_list<int>           il_s2  = {0, 1}; // { dg-warning "defined but not used" }
inline static std::initializer_list<int>    il_is1 = {0, 1}; // { dg-warning "defined but not used" }
inline static std::initializer_list<int>    il_is2 = {0, 1}; // { dg-warning "defined but not used" }
// No warnings:
constexpr static std::initializer_list<int> il_cs1 = {0, 1}; // { dg-bogus "defined but not used" }
constexpr static std::initializer_list<int> il_cs2 = {0, 1}; // { dg-bogus "defined but not used" }
std::initializer_list<int>                  il_1   = {0, 1}; // { dg-bogus "defined but not used" }
std::initializer_list<int>                  il_2   = {0, 1}; // { dg-bogus "defined but not used" }
inline std::initializer_list<int>           il_i1  = {0, 1}; // { dg-bogus "defined but not used" }
inline std::initializer_list<int>           il_i2  = {0, 1}; // { dg-bogus "defined but not used" }
constexpr std::initializer_list<int>        il_c1  = {0, 1}; // { dg-bogus "defined but not used" }
constexpr std::initializer_list<int>        il_c2  = {0, 1}; // { dg-bogus "defined but not used" }

// Warnings:
static auto           il_as1  = {0, 1}; // { dg-warning "defined but not used" }
static auto           il_as2  = {0, 1}; // { dg-warning "defined but not used" }
inline static auto    il_ais1 = {0, 1}; // { dg-warning "defined but not used" }
inline static auto    il_ais2 = {0, 1}; // { dg-warning "defined but not used" }
// No warnings:
constexpr static auto il_acs1 = {0, 1}; // { dg-bogus "defined but not used" }
constexpr static auto il_acs2 = {0, 1}; // { dg-bogus "defined but not used" }
auto                  il_a1   = {0, 1}; // { dg-bogus "defined but not used" }
auto                  il_a2   = {0, 1}; // { dg-bogus "defined but not used" }
inline auto           il_ai1  = {0, 1}; // { dg-bogus "defined but not used" }
inline auto           il_ai2  = {0, 1}; // { dg-bogus "defined but not used" }
constexpr auto        il_ac1  = {0, 1}; // { dg-bogus "defined but not used" }
constexpr auto        il_ac2  = {0, 1}; // { dg-bogus "defined but not used" }
