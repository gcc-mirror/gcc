/* { dg-do compile } */
/* { dg-options "-c -O3 -ftree-vectorize" { target *-*-* } } */

extern const int quant_coef8[6][8][8];
extern const int dequant_coef8[6][8][8];
int LevelScale8x8Luma_Intra[6][8][8];
int LevelScale8x8Luma_Inter[6][8][8];
int InvLevelScale8x8Luma_Intra[6][8][8];
int InvLevelScale8x8Luma_Inter[6][8][8];
short UseDefaultScalingMatrix8x8Flag[2];
void CalculateQuant8Param()
{
 int i, j, k, temp;
 int present[2];
 for(k=0; j<8; j++)
   for(i=0; i<8; i++)
     {
       temp = (i<<3)+j;
       if((!present[0]) || UseDefaultScalingMatrix8x8Flag[0])
         {
           LevelScale8x8Luma_Intra[k][j][i] = (quant_coef8[k][j][i]<<4);
           InvLevelScale8x8Luma_Intra[k][j][i] = dequant_coef8[k][j][i];
         }
       if((!present[1]) || UseDefaultScalingMatrix8x8Flag[1])
         {
           LevelScale8x8Luma_Inter[k][j][i] = (quant_coef8[k][j][i]<<4);
           InvLevelScale8x8Luma_Inter[k][j][i] = dequant_coef8[k][j][i];
         }
     }
}
