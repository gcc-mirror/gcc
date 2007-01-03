// PR c++/29535
// { dg-do compile }

template <class INDEX> struct SetRegion2D
{
  struct FloodFillControl
  {
    struct Allocator{};
  };
};
template <int DIM, class PIXELINDEX>
struct MotionSearcher
{
 typedef SetRegion2D<PIXELINDEX> Region_t;
 MotionSearcher (typename Region_t::FloodFillControl::Allocator &a_rAllocator);
};
class MotionSearcherY : public MotionSearcher<1, int> {};
