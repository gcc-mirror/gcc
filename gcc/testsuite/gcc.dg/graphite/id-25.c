typedef struct
{
  int mvd[2][(16/4)][(16/4)][2];
} Macroblock;
typedef struct
{
  int part_size[8][2];
} InputParameters;
typedef struct
{
  Macroblock *mb_data;
  short****** pred_mv;
  short****** all_mv;
} ImageParameters;
extern InputParameters *input;
extern ImageParameters *img;
int writeMotionVector8x8 (void)
{
  int i, j, k, l, m;
  int step_h = input->part_size[7][0];
  int step_v = input->part_size[7][1];
  Macroblock* currMB = &img->mb_data[9];
  int refindex = 0;
  short****** all_mv = img->all_mv;
  short****** pred_mv = img->pred_mv;

  for (k=0; k<2; k++)
    {
      int curr_mvd = all_mv[2][8][0][8][7][8] - pred_mv[2][7][0][8][7][0];
      for (l=0; l < step_v; l++)
        for (m=0; m < step_h; m++)
          currMB->mvd[0][8][9][8] = curr_mvd;
    }
}
