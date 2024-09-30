// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target vect_double }
// For MIN/MAX recognition
// { dg-additional-options "-ffast-math" }
// { dg-skip-if "requires hosted libstdc++ for cmath" { ! hostedlib } }

#include <algorithm>
#include <cmath>
#include <stdint.h>

// Point structure [x, y]
struct Point {
  double x, y;

  inline Point() noexcept = default;
  constexpr Point(const Point&) noexcept = default;

  constexpr Point(double x, double y) noexcept
    : x(x), y(y) {}
};

// Box structure [x0, y0, x1, y1]
struct Box {
  double x0, y0, x1, y1;

  inline void reset(double x0, double y0, double x1, double y1) noexcept {
    this->x0 = x0;
    this->y0 = y0;
    this->x1 = x1;
    this->y1 = y1;
  }
};

// Overloads to make vector processing simpler.
static constexpr Point operator-(const Point& a) noexcept { return Point(-a.x, -a.y); }

static constexpr Point operator+(const Point& a, double b) noexcept
{ return Point(a.x + b, a.y + b); }
static constexpr Point operator-(const Point& a, double b) noexcept
{ return Point(a.x - b, a.y - b); }
static constexpr Point operator*(const Point& a, double b) noexcept
{ return Point(a.x * b, a.y * b); }
static constexpr Point operator/(const Point& a, double b) noexcept
{ return Point(a.x / b, a.y / b); }

static constexpr Point operator+(const Point& a, const Point& b) noexcept
{ return Point(a.x + b.x, a.y + b.y); }
static constexpr Point operator-(const Point& a, const Point& b) noexcept
{ return Point(a.x - b.x, a.y - b.y); }
static constexpr Point operator*(const Point& a, const Point& b) noexcept
{ return Point(a.x * b.x, a.y * b.y); }
static constexpr Point operator/(const Point& a, const Point& b) noexcept
{ return Point(a.x / b.x, a.y / b.y); }

static constexpr Point operator+(double a, const Point& b) noexcept
{ return Point(a + b.x, a + b.y); }
static constexpr Point operator-(double a, const Point& b) noexcept
{ return Point(a - b.x, a - b.y); }
static constexpr Point operator*(double a, const Point& b) noexcept
{ return Point(a * b.x, a * b.y); }
static constexpr Point operator/(double a, const Point& b) noexcept
{ return Point(a / b.x, a / b.y); }

// Min/Max - different semantics compared to std.
template<typename T> constexpr T myMin(const T& a, const T& b) noexcept
{ return b < a ? b : a; }
template<typename T> constexpr T myMax(const T& a, const T& b) noexcept
{ return a < b ? b : a; }

// Linear interpolation, works with points as well.
template<typename V, typename T = double>
inline V lerp(const V& a, const V& b, const T& t) noexcept {
  return (a * (1.0 - t)) + (b * t);
}

// Merge a point into a box by possibly increasing its bounds.
inline void boxMergePoint(Box& box, const Point& p) noexcept {
  box.x0 = myMin(box.x0, p.x);
  box.y0 = myMin(box.y0, p.y);
  box.x1 = myMax(box.x1, p.x);
  box.y1 = myMax(box.y1, p.y);
}

void quadBoundingBoxA(const Point bez[3], Box& bBox) noexcept {
  // Bounding box of start and end points.
  bBox.reset(myMin(bez[0].x, bez[2].x), myMin(bez[0].y, bez[2].y),
             myMax(bez[0].x, bez[2].x), myMax(bez[0].y, bez[2].y));

  Point t = (bez[0] - bez[1]) / (bez[0] - bez[1] * 2.0 + bez[2]);

  t.x = myMax(t.x, 0.0);
  t.y = myMax(t.y, 0.0);
  t.x = myMin(t.x, 1.0);
  t.y = myMin(t.y, 1.0);

  boxMergePoint(bBox, lerp(lerp(bez[0], bez[1], t),
                               lerp(bez[1], bez[2], t), t));
}

// We should have if-converted everything down to straight-line code
// { dg-final { scan-tree-dump-times "<bb \[0-9\]+>" 1 "slp2" } }
// { dg-final { scan-tree-dump-times "Basic block will be vectorized using SLP" 1 "slp2" { xfail { { ! vect_element_align } && { ! vect_hw_misalign } } } } }
// It's a bit awkward to detect that all stores were vectorized but the
// following more or less does the trick
// { dg-final { scan-tree-dump "vect_\[^\r\m\]* = MIN" "slp2" { xfail { { ! vect_element_align } && { ! vect_hw_misalign } } } } }
