// { dg-do run }
// { dg-require-effective-target c++14 }
// { dg-options "-O2 -funroll-loops -fno-tree-vectorize" }

#include <vector>
#include <algorithm>
#include <cassert>

struct box {
   box(int xmin, int xmax, int ymin, int ymax) noexcept
      : m_xmin(xmin),
        m_xmax(xmax),
        m_ymin(ymin),
        m_ymax(ymax) {
   }

   box(box const & o) noexcept
      : m_xmin(o.m_xmin),
        m_xmax(o.m_xmax),
        m_ymin(o.m_ymin),
        m_ymax(o.m_ymax) { }

   int m_xmin;
   int m_xmax;
   int m_ymin;
   int m_ymax;
};


int main() {
    std::vector<box> vRects{ // requires 18 elements
        { 900, 11, 22, 33 },
        { 901, 11, 22, 33 },
        { 902, 11, 22, 33 },
        { 903, 11, 22, 33 },
        { 704, 11, 22, 33 },
        { 705, 11, 22, 33 },
        { 706, 11, 22, 33 },
        { 707, 11, 22, 33 },
        { 808, 11, 22, 33 },
        { 809, 11, 22, 33 },
        { 810, 11, 22, 33 },
        { 811, 11, 22, 33 },
        { 812, 11, 22, 33 },
        { 813, 11, 22, 33 },
        { 814, 11, 22, 33 },
        { 815, 11, 22, 33 },
        { 816, 11, 22, 33 },
        { 817, 11, 22, 33 },
        { 818, 11, 22, 33 },
    };

    std::stable_sort(vRects.begin(), vRects.end(),
		     [](auto const &r1, auto const &r2) -> bool
      {
        if (r2.m_xmax==0||r2.m_ymin==0||r2.m_ymax==0){__builtin_abort();}
        return r1.m_xmin < r2.m_xmin;
      });
  return 0;
}
