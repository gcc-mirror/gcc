// PR c++/118199
// { dg-do "compile" { target c++11 } }
// { dg-options "-fno-elide-constructors" } 

struct d { ~d(); };
d &b();
struct f {
  [[__no_unique_address__]] d e;
};
struct h : f  {
  h() : f{b()} {}
} i;
