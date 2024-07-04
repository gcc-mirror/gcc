/* This test used to ICE on PowerPC at -O due to combine GEN_INT bug.  */
typedef struct _ColRowInfo ColRowInfo;
typedef struct { }
GnmSheetRange;
struct _ColRowInfo
{
  float size_pts;
  unsigned margin_a:3;
  unsigned margin_b:3;
  unsigned visible:1;
};
int
colrow_equal (ColRowInfo const *a, ColRowInfo const *b)
{
  return a->size_pts == b->size_pts && a->margin_a == b->margin_a
    && a->visible == b->visible;
}

