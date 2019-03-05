// PR c++/60049 - Right and left shift undefined behavior not an error
//   in a constexpr
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-shift-negative-value -Wno-shift-count-negative -Wno-shift-count-overflow" }

constexpr int f1 (int n) { return 1 << n; }   // { dg-error "shift expression" }
constexpr int f2 (int n) { return 1 << n; }   // { dg-error "shift expression" }
constexpr int f3 (int n) { return n << 1; }   // { dg-error "shift expression" "" { target c++17_down } }
constexpr int f4 (int n) { return 1 >> n; }   // { dg-error "shift expression" }
constexpr int f5 (int n) { return 1 >> n; }   // { dg-error "shift expression" }

constexpr int X = __CHAR_BIT__ * sizeof (int) + 1;

constexpr int x1 = f1 (X);    // { dg-message "in .constexpr. expansion of" }
constexpr int x2 = f2 (-1);   // { dg-message "in .constexpr. expansion of" }
constexpr int x3 = f3 (-1);   // { dg-message "in .constexpr. expansion of" "" { target c++17_down } }
constexpr int x4 = f4 (X);    // { dg-message "in .constexpr. expansion of" }
constexpr int x5 = f5 (-1);   // { dg-message "in .constexpr. expansion of" }

constexpr int y1 =  1 << X;   // { dg-error "shift expression" }
constexpr int y2 =  1 << -1;  // { dg-error "shift expression" }
constexpr int y3 = -1 << 1;   // { dg-error "shift expression" "" { target c++17_down } }
constexpr int y4 =  1 >> X;   // { dg-error "shift expression" }
constexpr int y5 =  1 >> -1;  // { dg-error "shift expression" }
