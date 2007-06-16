/* ICE in chrec_fold_plus_poly_poly. */

typedef unsigned short __u16;
typedef unsigned int u32;
typedef __u16 __be16;
struct hfs_extent {
 __be16 count;
};
int hfs_free_fork( int type)
{
 u32 total_blocks, blocks, start;
 struct hfs_extent *extent;
 int res, i;
 for (i = 0; i < 3; extent++, i++)
  blocks += __fswab16((( __u16)(__be16)(extent[i].count)));
}
