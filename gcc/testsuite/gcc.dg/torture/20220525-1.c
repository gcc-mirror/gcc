/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

int LIST_1, mb_pred_b_d4x4spatial_dec_picture_l0_rFrame,
    mb_pred_b_d4x4spatial_dec_picture_l1_rFrame;
typedef struct {
    char ref_idx[2];
} PicMotionParams;
PicMotionParams mb_pred_b_d4x4spatial_dec_picture_mv_info;
int get_colocated_info_4x4___trans_tmp_1, get_colocated_info_4x4_list1_0;
int get_colocated_info_4x4()
{
  int moving =
      get_colocated_info_4x4_list1_0 && get_colocated_info_4x4___trans_tmp_1;
  return moving;
}
void mb_pred_b_d4x4spatial_dec_picture()
{
  char k;
  for (;;)
    {
      k = 0;
      for (; k < 4; k++)
        if (mb_pred_b_d4x4spatial_dec_picture_l0_rFrame
            || mb_pred_b_d4x4spatial_dec_picture_l1_rFrame == 0)
          {
            int is_not_moving = get_colocated_info_4x4();
            if (mb_pred_b_d4x4spatial_dec_picture_l1_rFrame)
              if (is_not_moving)
                mb_pred_b_d4x4spatial_dec_picture_mv_info.ref_idx[LIST_1] = 1;
          }
    }
}
