// { dg-do compile { target c++11 } }
// c++/81124 ICE with inline namespace

namespace std {
inline namespace {
int to_string();
void to_string(int);
}
void to_string();
}
int std::to_string();
