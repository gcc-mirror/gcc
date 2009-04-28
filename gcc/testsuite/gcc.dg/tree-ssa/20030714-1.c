/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
   
struct rtx_def;
typedef struct rtx_def *rtx;
enum rtx_code
{
  REG,
  LAST_AND_UNUSED_RTX_CODE = 256
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  enum rtx_code code:16;
  unsigned frame_related:1;
};

static rtx
find_base_value (src)
     rtx src;
{
  rtx temp;
  rtx src_0, src_2;
  rtx src_1, src_3;

  if ((src_0->code == REG) && (({src_2;})->frame_related))
    return find_base_value (src_0);
  if ((src_1->code == REG) && (({ src_3;})->frame_related))
    return find_base_value (src_1);
  if (src_0->code == REG)
    find_base_value (src_0);
  if (src_1->code == REG)
    find_base_value (src_1);
}

rtx
find_base_value_wrapper (src)
     rtx src;
{
  return find_base_value (src);
}

/* There should be no casts to short unsigned int.  */
/* { dg-final { scan-tree-dump-times "\\(short unsigned int\\)" 0 "dom2"} } */

/* { dg-final { cleanup-tree-dump "dom2" } } */
