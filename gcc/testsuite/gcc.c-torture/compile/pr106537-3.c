/* { dg-do compile } */
/* { dg-options "-O0 -Wno-compare-distinct-pointer-types" } */

typedef int __u32;

struct xdp_md
{
  char *data;
  char *data_meta;
};

int xdp_context (struct xdp_md *xdp)
{
  void *data = (void *)(long)xdp->data;
  __u32 *metadata = (void *)(long)xdp->data_meta;
  __u32 ret;

  if (metadata + 1 > data) /* There shouldn't be a warning here.  */
    return 1;
  if (metadata + 1 >= data) /* There shouldn't be a warning here. */
    return 2;
  if (metadata + 1 < data) /* There shouldn't be a warning here.  */
    return 3;
  if (metadata + 1 <= data) /* There shouldn't be a warning here.  */
    return 4;
  if (metadata + 1 == data) /* There shouldn't be a warning here.  */
    return 5;
  if (metadata + 1 != data) /* There shouldn't be a warning here.  */
    return 5;

  return 1;
}
