// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O -Wuninitialized" }

#include <vector>

struct Camera {
    struct P2d {
            float x, y;
    };
    std::vector<P2d> clip_area;
    float border = 10.f;
    [[gnu::noinline]] Camera() : clip_area({{border,border}}) { } // { dg-warning "uninitialized" }
};

Camera foo()
{
  Camera c;
  return c;
}
