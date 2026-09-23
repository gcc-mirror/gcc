// { dg-do run }
// { dg-additional-options "-std=c++23" }

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <span>
#include <vector>
#include <algorithm>

static constexpr std::size_t D = 3;
static constexpr std::size_t LEAF = 32;
static constexpr float INF = 1e15f;

struct LeafBucket {
     alignas(64) float coords[D][LEAF];
     uint32_t ids[LEAF];
};

struct FatPoint {
     float coords[D];
     unsigned payload_id;
};

struct Builder {
     std::span<FatPoint> temp_pts;
     std::span<float> vals;
     std::span<std::uint64_t> dims;
     std::span<LeafBucket> buckets;
     std::size_t B;

     void build(std::size_t start, std::size_t end, std::size_t 
node_idx, std::size_t current_buckets) {
         const std::size_t size = end - start;
         if (size == 0) return;
         if (node_idx >= B - 1) {
             const std::size_t bucket_idx = node_idx - (B - 1);
             for (std::size_t i = 0; i < size; ++i) {
                 for (std::size_t d = 0; d < D; ++d)
                     buckets[bucket_idx].coords[d][i] = temp_pts[start + 
i].coords[d];
                 buckets[bucket_idx].ids[i] = temp_pts[start + 
i].payload_id;
             }
             // Padding-fill loop below is the faulting loop.
             for (std::size_t i = size; i < LEAF; ++i) {
                 for (std::size_t d = 0; d < D; ++d)
                     buckets[bucket_idx].coords[d][i] = INF;
                 buckets[bucket_idx].ids[i] = 0xFFFFFFFFu;
             }
             return;
         }
         const std::size_t mid = start + current_buckets;
         std::nth_element(temp_pts.begin() + start, temp_pts.begin() + 
mid, temp_pts.begin() + end,
             [](const FatPoint& a, const FatPoint& b){ return 
a.coords[0] < b.coords[0]; });
         build(start, mid, 2 * node_idx + 1, 0);
         build(mid, end, 2 * node_idx + 2, 0);
     }
};

int main() {
     std::vector<LeafBucket> buckets(1);
     std::vector<FatPoint> pts(3);
     std::vector<float> vals(0);
     std::vector<std::uint64_t> dims(0);
     for (std::size_t i = 0; i < 3; ++i)
         for (std::size_t d = 0; d < D; ++d) pts[i].coords[d] = (float)i;
     Builder b{pts, vals, dims, buckets, 1};
     b.build(0, 3, 0, 1);
     return 0;
}
