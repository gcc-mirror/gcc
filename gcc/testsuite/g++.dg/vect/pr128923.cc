// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-fdump-tree-slp-details" }

struct Vec3 { float x, y, z; };
struct Point { Vec3 coord; int id; };
struct Box
{
  Vec3 min{ 3.4e38f, 3.4e38f, 3.4e38f }, max{ -3.4e38f, -3.4e38f, -3.4e38f };
  void include( const Vec3 & p )
    {
      min.x = p.x < min.x ? p.x : min.x; max.x = p.x > max.x ? p.x : max.x;
      min.y = p.y < min.y ? p.y : min.y; max.y = p.y > max.y ? p.y : max.y;
      min.z = p.z < min.z ? p.z : min.z; max.z = p.z > max.z ? p.z : max.z;
    }
};

Box boxOf( const Point * p, int n )
{
  Box b;
  for ( int i = 0; i < n; ++i )
    b.include( p[i].coord );
  return b;
}

// We want to either not vectorize or arrange to vectorize both a pair
// of stores to min and max, specifically avoid a vector load from p
// that is used in scalar form in the other half of the stores.  For this
// SLP discovery has to appropriately split up the 6-lane store group.
// { dg-final { scan-tree-dump-not "extracting lane for live stmt" "slp1" } }
