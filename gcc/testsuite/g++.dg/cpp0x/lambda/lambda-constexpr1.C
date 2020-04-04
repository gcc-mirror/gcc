// PR c++/94453
// { dg-do compile { target c++11 } }

void *ay();
template <typename f> f ay() { return *static_cast<f *>(ay()); }
template <typename h>
void bf() {
  ay<h>()();
}
struct az {
  template <typename h>
  az(h);
  using bk = void (*)();
  bk bl;
};
template <typename h>
az::az(h) { bl = bf<h>; }
struct A {};
void da(az);
void di(A, int);
void dk(A, az, az);
void b() {
  int data = 0;
  auto n = [] {};
  constexpr auto p = A{};
  auto q = [=] { di(p, data); };
  da([=] { dk(p, n, q); });
}
