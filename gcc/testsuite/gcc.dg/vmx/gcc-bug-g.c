/* { dg-do compile } */
#include <altivec.h>
extern vector  signed short table[8];
extern vector  signed short slope_cos[8];
extern vector  signed short slope_acos[8];

void Lsf_lsp(
  vector  signed short lsfq[],   
  vector  signed short lspq[]    
)
{
  vector  signed short Q17_con = ((vector signed short){20861,20861,20861,20861,20861,20861,20861,20861});
  vector  unsigned char perm1 = ((vector  unsigned char){0, 8, 1, 9, 2, 10, 3, 
  											11, 4, 12, 5 ,13, 6, 14, 7, 15});
  vector  unsigned char PerIndex, tmpIndex;
  vector  signed short tmp0, tmp1, tmp2, tmp3;
  vector  signed short stmp0, stmp1, stmp2, stmp3;
  vector  signed short index0, index1, offset0, offset1;
  vector  signed short table0, table1, slope0, slope1;
  vector  unsigned short select;
  vector  signed int L_tmp0, L_tmp1, L_tmp2, L_tmp3;
 
   
  tmp0 = vec_madds(lsfq[0], Q17_con, (((vector signed short){0,0,0,0,0,0,0,0})) );
  tmp1 = vec_madds(lsfq[1], Q17_con, (((vector signed short){0,0,0,0,0,0,0,0})) );

   
  offset0 = vec_and(tmp0, (((vector signed short){0x00ff,0x00ff,0x00ff,0x00ff,0x00ff,0x00ff,0x00ff,0x00ff})) );
  offset1 = vec_and(tmp1, (((vector signed short){0x00ff,0x00ff,0x00ff,0x00ff,0x00ff,0x00ff,0x00ff,0x00ff})) );
  
   
  index0 = vec_min(vec_sra(tmp0, (((vector unsigned short){8,8,8,8,8,8,8,8})) ), (((vector signed short){63,63,63,63,63,63,63,63})) );
  index1 = vec_min(vec_sra(tmp1, (((vector unsigned short){8,8,8,8,8,8,8,8})) ), (((vector signed short){63,63,63,63,63,63,63,63})) );
  
   
   
  tmp0 = vec_sl(index0, (vector  unsigned short)((((vector signed short){1,1,1,1,1,1,1,1})) ));
  PerIndex = (vector  unsigned char)vec_packs(tmp0, vec_add(tmp0, (((vector signed short){1,1,1,1,1,1,1,1})) ));
  PerIndex = vec_perm(PerIndex, PerIndex, perm1);
  
    
  tmp0 = vec_perm(table[0], table[1], PerIndex);
  stmp0 = vec_perm(slope_cos[0], slope_cos[1], PerIndex);

  tmpIndex = vec_sub(PerIndex, (((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32})) );
  tmp1 = vec_perm(table[2], table[3], tmpIndex);
  stmp1 = vec_perm(slope_cos[2], slope_cos[3], tmpIndex);

  select = (vector  unsigned short)vec_cmpgt(PerIndex, (((vector unsigned char){31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31})) );
  tmp2 = vec_sel(tmp0, tmp1, select);
  stmp2 = vec_sel(stmp0, stmp1, select);

  tmpIndex = vec_sub(tmpIndex, (((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32})) );
  tmp0 = vec_perm(table[4], table[5], tmpIndex);
  stmp0 = vec_perm(slope_cos[4], slope_cos[5], tmpIndex);
  
  tmpIndex = vec_sub(tmpIndex, (((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32})) );
  tmp1 = vec_perm(table[6], table[7], tmpIndex);
  stmp1 = vec_perm(slope_cos[6], slope_cos[7], tmpIndex);

  select = (vector  unsigned short)vec_cmpgt(PerIndex, (((vector unsigned char){95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95})) );
  tmp3 = vec_sel(tmp0, tmp1, select);
  stmp3 = vec_sel(stmp0, stmp1, select);
  
  select = (vector  unsigned short)vec_cmpgt(PerIndex, (((vector unsigned char){63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63})) );
  table0 = vec_sel(tmp2, tmp3, select);
  slope0 = vec_sel(stmp2, stmp3, select);

  tmp0 = vec_sl(index1, (vector  unsigned short)((((vector signed short){1,1,1,1,1,1,1,1})) ));
  PerIndex = (vector  unsigned char)vec_packs(tmp0, vec_add(tmp0, (((vector signed short){1,1,1,1,1,1,1,1})) ));
  PerIndex = vec_perm(PerIndex, PerIndex, perm1);

   
  tmp0 = vec_perm(table[0], table[1], PerIndex);
  stmp0 = vec_perm(slope_cos[0], slope_cos[1], PerIndex);

  tmpIndex = vec_sub(PerIndex, (((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32})) );
  tmp1 = vec_perm(table[2], table[3], tmpIndex);
  stmp1 = vec_perm(slope_cos[2], slope_cos[3], tmpIndex);

  select = (vector  unsigned short)vec_cmpgt(PerIndex, (((vector unsigned char){31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31})) );
  tmp2 = vec_sel(tmp0, tmp1, select);
  stmp2 = vec_sel(stmp0, stmp1, select);

  tmpIndex = vec_sub(tmpIndex, (((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32})) );
  tmp0 = vec_perm(table[4], table[5], tmpIndex);
  stmp0 = vec_perm(slope_cos[4], slope_cos[5], tmpIndex);
  
  tmpIndex = vec_sub(tmpIndex, (((vector unsigned char){32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32})) );
  tmp1 = vec_perm(table[6], table[7], tmpIndex);
  stmp1 = vec_perm(slope_cos[6], slope_cos[7], tmpIndex);

  select = (vector  unsigned short)vec_cmpgt(PerIndex, (((vector unsigned char){95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95})) );
  tmp3 = vec_sel(tmp0, tmp1, select);
  stmp3 = vec_sel(stmp0, stmp1, select);
  
  select = (vector  unsigned short)vec_cmpgt(PerIndex, (((vector unsigned char){63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63})) );
  table1 = vec_sel(tmp2, tmp3, select);
  slope1 = vec_sel(stmp2, stmp3, select);


   
  L_tmp0 = vec_sra(vec_mule(slope0, offset0), (((vector unsigned int){12,12,12,12})) );
  L_tmp1 = vec_sra(vec_mulo(slope0, offset0), (((vector unsigned int){12,12,12,12})) );
  L_tmp2 = vec_sra(vec_mule(slope1, offset1), (((vector unsigned int){12,12,12,12})) );
  L_tmp3 = vec_sra(vec_mulo(slope1, offset1), (((vector unsigned int){12,12,12,12})) );

  
  tmp0 = vec_packs(L_tmp0, L_tmp2);
  tmp1 = vec_packs(L_tmp1, L_tmp3);
  tmp2 = vec_mergeh(tmp0, tmp1);
  tmp3 = vec_mergel(tmp0, tmp1);
  
   
  lspq[0] = vec_adds(table0, tmp2);
  lspq[1] = vec_adds(table1, tmp3);

  return;
}
