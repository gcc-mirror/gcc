/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2 -Wno-deprecated" } */

#include <altivec.h>

int printf(const char * , ...);
extern void abort();

void foo(char *bS, char *bS_edge, int field_MBAFF, int top){
  char intra[16]       __attribute__ ((aligned(16)));
  signed short mv_const[8] __attribute__((aligned(16)));
  
  vector signed short v_three, v_ref_mask00, v_ref_mask01, v_vec_maskv, v_vec_maskh;
  vector unsigned char v_permv, v_permh, v_bS, v_bSh, v_bSv, v_cbp_maskv, v_cbp_maskvn, v_cbp_maskh, v_cbp_maskhn, v_intra_maskh, v_intra_maskv, v_intra_maskhn, v_intra_maskvn;
  vector unsigned char tmp7, tmp8, tmp9, tmp10, v_c1, v_cbp1, v_cbp2, v_pocl, v_poch;
  vector signed short v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
  vector signed short idx0;
  vector signed short tmp00, tmp01, tmp02, tmp03;
  vector unsigned char v_zero   = (vector unsigned char) {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'};
  v_three  = (vector signed short) vec_ld (0, (vector signed short *) mv_const);

  vector unsigned char v_coef_mask = vec_ld(0, (vector unsigned char *)mv_const);
  vector unsigned char v_coef_mask_hi = vec_splat(v_coef_mask, 0);
  vector unsigned char v_coef_mask_lo = vec_splat(v_coef_mask, 1);
  v_coef_mask = vec_sld(v_coef_mask_hi, v_coef_mask_lo, 8);
  vector unsigned char v_bit_mask = vec_sub(vec_splat_u8(7), vec_lvsl(0, (unsigned char *)0));
  v_bit_mask  = vec_sld(vec_sld(v_bit_mask, v_bit_mask, 8), v_bit_mask, 8);
  v_bit_mask  = vec_sl(vec_splat_u8(1), v_bit_mask);
  tmp5        = (vector signed short) vec_and(v_coef_mask, v_bit_mask);

  intra[0] = 1;
  tmp8     = vec_ld (0, (vector unsigned char *) intra);
  tmp9     = vec_ld (0, (vector unsigned char *) mv_const);
  tmp10    = vec_ld (0, (vector unsigned char *) mv_const);
  v_permv  = vec_ld (0, (vector unsigned char *) mv_const);
  v_permh  = vec_ld (0, (vector unsigned char *) mv_const);
  tmp6     = vec_ld (0, (vector signed short *) mv_const);

  tmp8     = vec_splat((vector unsigned char) tmp8, 0);
  tmp9     = vec_splat((vector unsigned char) tmp9, 12);
  tmp10    = vec_splat((vector unsigned char) tmp10, 12);
  tmp9     = vec_sld ((vector unsigned char) tmp9,(vector unsigned char) tmp8, 12);
  tmp10    = vec_sld ((vector unsigned char) tmp10, (vector unsigned char) tmp8, 12);
  v_intra_maskv  = vec_or (tmp9, tmp8);
  v_intra_maskh  = vec_or (tmp10, tmp8);
  v_intra_maskv  = (vector unsigned char) vec_cmpgt ((vector unsigned char) v_intra_maskv, (vector unsigned char) v_zero);
  v_intra_maskh  = (vector unsigned char) vec_cmpgt ((vector unsigned char) v_intra_maskh, (vector unsigned char) v_zero);

  tmp9   = vec_lvsl (4 + (top<<2), (unsigned char *) 0x0);
  v_cbp1 = vec_perm ((vector unsigned char) tmp6, (vector unsigned char) tmp6, tmp9);
  v_cbp2 = (vector unsigned char) vec_perm ((vector unsigned char) tmp5, (vector unsigned char) tmp5, (vector unsigned char) v_permv);
  v_cbp1 = (vector unsigned char) vec_sld  ((vector unsigned char) v_cbp1,(vector unsigned char) v_cbp2, 12);
  v_cbp_maskv = vec_or (v_cbp1, v_cbp2);

  tmp9   = vec_lvsl (12 + (top<<2), (unsigned char *) 0x0);
  v_cbp1 = vec_perm ((vector unsigned char) tmp6, (vector unsigned char) tmp6, tmp9);
  v_cbp2 = (vector unsigned char) vec_perm ((vector unsigned char) tmp5, (vector unsigned char) tmp5, (vector unsigned char) v_permh);
  v_cbp1 = (vector unsigned char) vec_sld  ((vector unsigned char) v_cbp1,(vector unsigned char) v_cbp2, 12);
  v_cbp_maskh = vec_or (v_cbp1, v_cbp2);

  v_cbp_maskv = (vector unsigned char) vec_cmpgt ((vector unsigned char) v_cbp_maskv, (vector unsigned char) v_zero);
  v_cbp_maskh = (vector unsigned char) vec_cmpgt ((vector unsigned char) v_cbp_maskh, (vector unsigned char) v_zero);

  intra[0]  =0; 
  intra[1]  =1;
  intra[2]  =2;
  intra[3]  =3;
  intra[4]  =4;
  intra[5]  = 5;
  intra[6]  =6;
  intra[7]  =7;
  intra[8]  =8;
  intra[9]  =9;
  intra[10] =9;
  intra[11] =9;
  intra[12] = 0xff;

  idx0   = vec_ld (0, (signed short *) intra);
  
  v_c1   = (vector unsigned char)  {'1','2','3','4','5','6','7','8','1','2','3','4','5','6','7','8'};

  if (field_MBAFF){
    v0   = (vector signed short) vec_and ((vector unsigned char) idx0, v_c1);
    idx0 = (vector signed short) vec_sra ((vector unsigned char) idx0, v_c1);

    v1   = vec_sld (v0, v0, 15);
    v1   = (vector signed short) vec_pack (v1, v0);
    
    v2   = vec_sld (v1, v1, 2);
    v3   = vec_sld (v1, v1, 10);
    
    v4   = (vector signed short) vec_cmpeq ((vector signed char) v1, (vector signed char) v2);
    v5   = (vector signed short) vec_cmpeq ((vector signed char) v1, (vector signed char) v3);
    v6   = (vector signed short) vec_cmpeq ((vector signed char) v2, (vector signed char) v3);
  }
  else  {
    v4 = v5 = v6 = (vector signed short) vec_nor (v_zero, v_zero);
  }

  tmp1   = (vector signed short) vec_sl ((vector unsigned char) idx0, v_c1);
  v_c1   = vec_mergeh ((vector unsigned char) v_zero, v_c1);
  tmp1   = (vector signed short) vec_add (tmp1, (vector signed short) v_c1); 

  v_pocl = vec_ld (0, (vector unsigned char *) mv_const);
  v_poch = vec_ld (0, (vector unsigned char *) mv_const);
  tmp2   = (vector signed short) vec_perm (v_pocl, v_poch, (vector unsigned char) tmp1);      

  v_pocl = vec_ld (0,  (vector unsigned char *) mv_const);
  v_poch = vec_ld (16, (vector unsigned char *) mv_const);
  tmp1   = (vector signed short) vec_perm (v_pocl, v_poch, (vector unsigned char) tmp1);
  tmp1   = vec_sel (tmp1, tmp2, (vector unsigned short) {0xffff,0xffff,0,0,0,0,0,0});

  tmp3   = (vector signed short) vec_splat ((vector unsigned char) idx0, 12);
  v_c1   = (vector unsigned char) vec_nor (v_zero, v_zero);
  tmp0   = (vector signed short) vec_cmpeq ((vector signed char) idx0, (vector signed char) v_c1);
  tmp1   = vec_sel (tmp1, (vector signed short) tmp3, (vector unsigned short) tmp0);

  tmp2   = vec_sld (tmp1, tmp1, 15);
  tmp1   = (vector signed short) vec_pack (tmp2, tmp1);
  
  tmp2   = vec_sld (tmp1, tmp1, 2);
  tmp3   = vec_sld (tmp1, tmp1, 10);

  tmp0   = (vector signed short) vec_cmpeq ((vector signed char) tmp1, (vector signed char) tmp2);
  tmp4   = (vector signed short) vec_cmpeq ((vector signed char) tmp1, (vector signed char) tmp3);
  tmp1   = (vector signed short) vec_cmpeq ((vector signed char) tmp2, (vector signed char) tmp3);
  tmp0   = vec_and (tmp0, v4);
  tmp4   = vec_and (tmp4, v5);
  tmp1   = vec_and (tmp1, v6);
  tmp2   = vec_sld ((vector signed short) tmp0, (vector signed short) tmp0, 8);
  tmp3   = vec_sld ((vector signed short) tmp4, (vector signed short) tmp4, 8);
  tmp5   = vec_sld ((vector signed short) tmp1, (vector signed short) tmp1, 8);
  tmp0   = vec_and (tmp0, tmp2);
  tmp4   = vec_and (tmp4, tmp3);
  tmp1   = vec_and (tmp1, tmp5);
  v_ref_mask00 = vec_mergeh ((vector signed short) tmp0, (vector signed short) v_c1);
  v_ref_mask01 = vec_mergeh ((vector signed short) tmp4, (vector signed short) tmp1);
  v_ref_mask00 = (vector signed short) vec_mergeh ((vector unsigned char) v_ref_mask00, (vector unsigned char) v_ref_mask00);
  v_ref_mask01 = (vector signed short) vec_mergeh ((vector unsigned char) v_ref_mask01, (vector unsigned char) v_ref_mask01);
  
  v0     = vec_ld (0,  (vector signed short *) mv_const);
  v1     = vec_ld (16, (vector signed short *) mv_const);
  v4     = vec_ld (64, (vector signed short *) mv_const);
  v5     = vec_ld (80, (vector signed short *) mv_const);
  v8     = vec_ld (0,  (vector signed short *) mv_const);
  v9     = vec_ld (16, (vector signed short *) mv_const);

  tmp0   = (vector signed short) vec_perm ((vector unsigned char) v8, 
		(vector unsigned char) v8, (vector unsigned char) {0,1,2,3,8,9,10,11,4,5,6,7,12,13,14,15});
  tmp1   = (vector signed short) vec_mergeh ((vector signed int) v0, (vector signed int) v1);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp3   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp4   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp0   = (vector signed short) vec_perm ((vector unsigned char) v9, (vector unsigned char) v9, 
			(vector unsigned char) {0,1,2,3,8,9,10,11,4,5,6,7,12,13,14,15});
  tmp1   = (vector signed short) vec_mergeh ((vector signed int) v4, (vector signed int) v5);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp5   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp6   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 12);
  tmp6   = vec_sld (tmp4, tmp4, 12);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp00  = (vector signed short) vec_pack ((vector unsigned short) tmp3, (vector unsigned short) tmp4);

  tmp0   = (vector signed short) vec_mergeh ((vector signed int) v0, (vector signed int) v1);
  tmp1   = (vector signed short) vec_mergel ((vector signed int) v0, (vector signed int) v1);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp3   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp4   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp0   = (vector signed short) vec_mergeh ((vector signed int) v4, (vector signed int) v5);
  tmp1   = (vector signed short) vec_mergel ((vector signed int) v4, (vector signed int) v5);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp5   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp6   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 12);
  tmp6   = vec_sld (tmp4, tmp4, 12);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp01  = (vector signed short) vec_pack ((vector unsigned short) tmp3, (vector unsigned short) tmp4);

  v2     = vec_ld (32, (vector signed short *) mv_const);
  v3     = vec_ld (48, (vector signed short *) mv_const);
  v6     = vec_ld (96, (vector signed short *) mv_const);
  v7     = vec_ld (112,(vector signed short *) mv_const);

  tmp0   = (vector signed short) vec_mergel ((vector signed int) v0, (vector signed int) v1);
  tmp1   = (vector signed short) vec_mergeh ((vector signed int) v2, (vector signed int) v3);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp3   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp4   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp0   = (vector signed short) vec_mergel ((vector signed int) v4, (vector signed int) v5);
  tmp1   = (vector signed short) vec_mergeh ((vector signed int) v6, (vector signed int) v7);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp5   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp6   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 12);
  tmp6   = vec_sld (tmp4, tmp4, 12);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp02  = (vector signed short) vec_pack ((vector unsigned short) tmp3, (vector unsigned short) tmp4);

  tmp0   = (vector signed short) vec_mergeh ((vector signed int) v2, (vector signed int) v3);
  tmp1   = (vector signed short) vec_mergel ((vector signed int) v2, (vector signed int) v3);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp3   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp4   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp0   = (vector signed short) vec_mergeh ((vector signed int) v6, (vector signed int) v7);
  tmp1   = (vector signed short) vec_mergel ((vector signed int) v6, (vector signed int) v7);
  tmp2   = vec_sld (tmp1, tmp1, 8);
  tmp5   = vec_sub (vec_max (tmp0, tmp1), vec_min (tmp0, tmp1));
  tmp6   = vec_sub (vec_max (tmp0, tmp2), vec_min (tmp0, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 12);
  tmp6   = vec_sld (tmp4, tmp4, 12);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp03  = (vector signed short) vec_pack ((vector unsigned short) tmp3, (vector unsigned short) tmp4);

  tmp0   = (vector signed short) vec_pack ((vector unsigned int) tmp00, (vector unsigned int) tmp01);
  tmp1   = (vector signed short) vec_pack ((vector unsigned int) tmp02, (vector unsigned int) tmp03);
  tmp2   = (vector signed short) vec_mergeh ((vector signed int) tmp0, (vector signed int) tmp1);
  tmp3   = (vector signed short) vec_mergel ((vector signed int) tmp0, (vector signed int) tmp1);
  tmp4   = (vector signed short) vec_mergeh ((vector signed int) tmp2, (vector signed int) tmp3);
  tmp5   = (vector signed short) vec_mergel ((vector signed int) tmp2, (vector signed int) tmp3);
  tmp4   = vec_and (v_ref_mask00, tmp4);
  tmp5   = vec_and (v_ref_mask01, tmp5);

  tmp0   = vec_nor (v_ref_mask00, v_ref_mask01);
  tmp1   = vec_and (v_ref_mask00, v_ref_mask01);
  tmp2   = vec_and (tmp4, tmp5);
  tmp2   = vec_and (tmp2, tmp1);
  tmp3   = vec_nor (tmp4, tmp5);
  tmp3   = vec_nor (tmp3, tmp1);
  v_vec_maskv = vec_or (tmp0, tmp2);
  v_vec_maskv = vec_or (v_vec_maskv, tmp3);

  intra[0]  = 1; 
  intra[1]  = 1;
  intra[2]  = 2;
  intra[3]  = 3;
  intra[4]  = 2;
  intra[5]  = 2;
  intra[6]  = 2;
  intra[7]  = 1;
  intra[8]  = 1;
  intra[9]  = 5;
  intra[10] = 5;
  intra[11] = 5;
  
  intra[13] = 0;
  intra[14] = 0;
  intra[15] = 0;

  idx0   = vec_ld (0, (signed short *) intra);
  
  v_c1   = (vector unsigned char)  {'1','2','3','4','5','6','7','8','1','2','3','4','5','6','7','8'};

  if (field_MBAFF){
    v8   = (vector signed short) vec_and ((vector unsigned char) idx0, v_c1);
    idx0 = (vector signed short) vec_sra ((vector unsigned char) idx0, v_c1);

    v9   = vec_sld (v8, v8, 15);
    v9   = (vector signed short) vec_pack (v9, v8);
    
    v10  = vec_sld (v9, v9, 2);
    v11  = vec_sld (v9, v9, 10);
    
    v8   = (vector signed short) vec_cmpeq ((vector signed char) v9, (vector signed char) v10);
    v9   = (vector signed short) vec_cmpeq ((vector signed char) v9, (vector signed char) v11);
    v10  = (vector signed short) vec_cmpeq ((vector signed char) v10, (vector signed char) v11);
  }
  else  {
    v8 = v9 = v10 = (vector signed short) vec_nor (v_zero, v_zero);
  }

  tmp1   = (vector signed short) vec_sl ((vector unsigned char) idx0, v_c1);
  
if (1){
	int m;
	unsigned char toto2[16] __attribute__((aligned(16)));
	
	printf("vc1\n");
	vec_st(v_c1, 0, (unsigned char *) toto2);
	for (m=0; m<16;m++) {printf("%c ", toto2[m]);}
	
	printf("\nv_zero\n");
	
	vec_st (v_zero, 0, (unsigned char *) toto2);
	for (m=0; m< 16; m++) {printf("%c ", toto2[m]);}
	printf("\n");
}

  v_c1   = vec_mergeh ((vector unsigned char) v_zero, v_c1);
  tmp1   = (vector signed short) vec_add (tmp1, (vector signed short) v_c1); 

if (1){
	vector unsigned char vres = 
        (vector unsigned char){'a','1','b','2','c','3','d','4','e','5','f','6','g','7','h','8'};
	unsigned char toto2[16] __attribute__((aligned(16)));
	int m;
	
	printf("vc1\n");
	vec_st(v_c1, 0, (unsigned char *) toto2);
	for (m=0; m<16;m++) {printf("%c ", toto2[m]);}
	printf("\n");
	if (!vec_all_eq (vres, v_c1))
	  abort();
}

  v_pocl = vec_ld (32, (vector unsigned char *) mv_const);
  v_poch = vec_ld (48, (vector unsigned char *) mv_const);
  tmp2   = (vector signed short) vec_perm (v_pocl, v_poch, (vector unsigned char) tmp1);      

  v_pocl = vec_ld (0,  (vector unsigned char *) mv_const);
  v_poch = vec_ld (16, (vector unsigned char *) mv_const);

  tmp1   = (vector signed short) vec_perm (v_pocl, v_poch, (vector unsigned char) tmp1);
 
  tmp1   = vec_sel (tmp1, tmp2, (vector unsigned short) {0xffff,0xffff,0,0,0,0,0,0});


  tmp3   = (vector signed short) vec_splat ((vector unsigned char) idx0, 12);
  v_c1   = (vector unsigned char) vec_nor (v_zero, v_zero);
  tmp0   = (vector signed short) vec_cmpeq ((vector signed char) idx0, (vector signed char) v_c1);
  tmp1   = vec_sel (tmp1, (vector signed short) tmp3, (vector unsigned short) tmp0);

  tmp2   = vec_sld (tmp1, tmp1, 15);
  tmp1   = (vector signed short) vec_pack (tmp2, tmp1);
  

  tmp2   = vec_sld (tmp1, tmp1, 2);
  tmp3   = vec_sld (tmp1, tmp1, 10);

  tmp0   = (vector signed short) vec_cmpeq ((vector signed char) tmp1, (vector signed char) tmp2);
  tmp4   = (vector signed short) vec_cmpeq ((vector signed char) tmp1, (vector signed char) tmp3);
  tmp1   = (vector signed short) vec_cmpeq ((vector signed char) tmp2, (vector signed char) tmp3);
  tmp0   = vec_and (tmp0, v8);
  tmp4   = vec_and (tmp4, v9);
  tmp1   = vec_and (tmp1, v10);
  tmp2   = vec_sld ((vector signed short) tmp0, (vector signed short) tmp0, 8);
  tmp3   = vec_sld ((vector signed short) tmp4, (vector signed short) tmp4, 8);
  tmp5   = vec_sld ((vector signed short) tmp1, (vector signed short) tmp1, 8);
  tmp0   = vec_and (tmp0, tmp2);
  tmp4   = vec_and (tmp4, tmp3);
  tmp1   = vec_and (tmp1, tmp5);
  v_ref_mask00 = vec_mergeh ((vector signed short) tmp0, (vector signed short) v_c1);
  v_ref_mask01 = vec_mergeh ((vector signed short) tmp4, (vector signed short) tmp1);
  v_ref_mask00 = (vector signed short) vec_mergeh ((vector unsigned char) v_ref_mask00, (vector unsigned char) v_ref_mask00);
  v_ref_mask01 = (vector signed short) vec_mergeh ((vector unsigned char) v_ref_mask01, (vector unsigned char) v_ref_mask01);
  

  v_permv= vec_ld (0, (vector unsigned char *) mv_const);
  v8     = vec_ld (0,  (vector signed short *) mv_const);
  v9     = vec_ld (16, (vector signed short *) mv_const);
  tmp2   = vec_perm (v0, v0, v_permv);
  tmp3   = vec_sub (vec_max (v8, v0), vec_min (v8, v0));
  tmp4   = vec_sub (vec_max (v8, tmp2), vec_min (v8, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp2   = vec_perm (v2, v2, v_permv);
  tmp5   = vec_sub (vec_max (v9, v2), vec_min (v9, v2));
  tmp6   = vec_sub (vec_max (v9, tmp2), vec_min (v9, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp00  = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp4);

  tmp2   = vec_perm (v1, v1, v_permv);
  tmp3   = vec_sub (vec_max (v0, v1), vec_min (v0, v1));
  tmp4   = vec_sub (vec_max (v0, tmp2), vec_min (v0, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp2   = vec_perm (v3, v3, v_permv);
  tmp5   = vec_sub (vec_max (v2, v3), vec_min (v2, v3));
  tmp6   = vec_sub (vec_max (v2, tmp2), vec_min (v2, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp01  = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp4);

  tmp2   = vec_perm (v4, v4, v_permv);
  tmp3   = vec_sub (vec_max (v1, v4), vec_min (v1, v4));
  tmp4   = vec_sub (vec_max (v1, tmp2), vec_min (v1, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp2   = vec_perm (v6, v6, v_permv);
  tmp5   = vec_sub (vec_max (v3, v6), vec_min (v3, v6));
  tmp6   = vec_sub (vec_max (v3, tmp2), vec_min (v3, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp02  = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp4);


  tmp2   = vec_perm (v5, v5, v_permv);
  tmp3   = vec_sub (vec_max (v4, v5), vec_min (v4, v5));
  tmp4   = vec_sub (vec_max (v4, tmp2), vec_min (v4, tmp2));
  tmp3   = (vector signed short) vec_cmpgt (tmp3, v_three); 
  tmp4   = (vector signed short) vec_cmpgt (tmp4, v_three);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  
  tmp2   = vec_perm (v7, v7, v_permv);
  tmp5   = vec_sub (vec_max (v6, v7), vec_min (v6, v7));
  tmp6   = vec_sub (vec_max (v6, tmp2), vec_min (v6, tmp2));
  tmp5   = (vector signed short) vec_cmpgt (tmp5, v_three); 
  tmp6   = (vector signed short) vec_cmpgt (tmp6, v_three);
  tmp0   = vec_sld (tmp5, tmp5, 14);
  tmp1   = vec_sld (tmp6, tmp6, 14);
  tmp5   = vec_or (tmp0, tmp5);
  tmp6   = vec_or (tmp1, tmp6);
  
  tmp3   = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp5);
  tmp4   = (vector signed short) vec_pack ((vector unsigned int) tmp4, (vector unsigned int) tmp6);
  tmp5   = vec_sld (tmp3, tmp3, 14);
  tmp6   = vec_sld (tmp4, tmp4, 14);
  tmp3   = vec_or (tmp3, tmp5);
  tmp4   = vec_or (tmp4, tmp6);
  tmp03  = (vector signed short) vec_pack ((vector unsigned int) tmp3, (vector unsigned int) tmp4);

  tmp0   = (vector signed short) vec_pack ((vector unsigned short) tmp00, (vector unsigned short) tmp01);
  tmp1   = (vector signed short) vec_pack ((vector unsigned short) tmp02, (vector unsigned short) tmp03);
  tmp2   = (vector signed short) vec_mergeh ((vector signed int) tmp0, (vector signed int) tmp1);
  tmp3   = (vector signed short) vec_mergel ((vector signed int) tmp0, (vector signed int) tmp1);
  tmp4   = (vector signed short) vec_mergeh ((vector signed int) tmp2, (vector signed int) tmp3);
  tmp5   = (vector signed short) vec_mergel ((vector signed int) tmp2, (vector signed int) tmp3);
  tmp4   = vec_and (v_ref_mask00, tmp4);
  tmp5   = vec_and (v_ref_mask01, tmp5);

  tmp0   = vec_nor (v_ref_mask00, v_ref_mask01);
  tmp1   = vec_and (v_ref_mask00, v_ref_mask01);
  tmp2   = vec_and (tmp4, tmp5);
  tmp2   = vec_and (tmp2, tmp1);
  tmp3   = vec_nor (tmp4, tmp5);
  tmp3   = vec_nor (tmp3, tmp1);
  v_vec_maskh = vec_or (tmp0, tmp2);
  v_vec_maskh = vec_or (v_vec_maskh, tmp3);


  v_intra_maskvn = vec_nor (v_intra_maskv, v_intra_maskv);
  v_intra_maskhn = vec_nor (v_intra_maskh, v_intra_maskh);
  v_cbp_maskvn = (vector unsigned char) vec_cmpeq ((vector unsigned char) v_cbp_maskv, (vector unsigned char) v_zero);
  v_cbp_maskhn = (vector unsigned char) vec_cmpeq ((vector unsigned char) v_cbp_maskh, (vector unsigned char) v_zero);

  v_cbp_maskv  = vec_and (v_cbp_maskv, v_intra_maskvn);
  v_cbp_maskh  = vec_and (v_cbp_maskh, v_intra_maskhn);
  v_vec_maskv  = vec_and (v_vec_maskv, (vector signed short) v_intra_maskvn);
  v_vec_maskv  = vec_and (v_vec_maskv, (vector signed short) v_cbp_maskvn);
  v_vec_maskh  = vec_and (v_vec_maskh, (vector signed short) v_intra_maskhn);
  v_vec_maskh  = vec_and (v_vec_maskh, (vector signed short) v_cbp_maskhn);

  tmp9        = vec_splat_u8(2);
  tmp8        = vec_splat_u8(1);
  v_bS        = vec_ld (0, (vector unsigned char *) mv_const);
  
  v_bSv       = vec_and ((vector unsigned char) v_bS, (vector unsigned char)v_intra_maskv);
  tmp7        = vec_and ((vector unsigned char)tmp9, (vector unsigned char)v_cbp_maskv);
  tmp6        = (vector signed short) vec_and ((vector unsigned char)tmp8, (vector unsigned char)v_vec_maskv);
  tmp7        = vec_or  ((vector unsigned char)tmp7, (vector unsigned char)tmp6);
  v_bSv       = vec_or  ((vector unsigned char)tmp7, (vector unsigned char)v_bSv);

  v_bS        = vec_ld (0, (vector unsigned char *) mv_const);
  v_bSh       = vec_and ((vector unsigned char) v_bS, (vector unsigned char)v_intra_maskh);
  tmp7        = vec_and ((vector unsigned char)tmp9, (vector unsigned char)v_cbp_maskh);
  tmp6        = (vector signed short) vec_and ((vector unsigned char)tmp8, (vector unsigned char)v_vec_maskh);
  tmp7        = vec_or  ((vector unsigned char)tmp7, (vector unsigned char)tmp6);
  v_bSh       = vec_or  ((vector unsigned char)tmp7, (vector unsigned char)v_bSh);

  v_permh     = (vector unsigned char) vec_ld (0 , (vector unsigned char *) mv_const);
  v_permv     = (vector unsigned char) vec_ld (0, (vector unsigned char *) mv_const);
  v_bSv       = vec_and (v_bSv, v_permv);
  v_bSh       = vec_and (v_bSh, v_permh);

  vec_st (v_bSv, 0, (unsigned char *) mv_const);
  vec_st (v_bSh, 0, (unsigned char *) mv_const);

  v_bSv = vec_mergeh (v_bSv, v_bSv);
  v_bSv = vec_mergeh (v_bSv, v_bSv);
  v_bSh = vec_mergeh (v_bSh, v_bSh);
  v_bSh = vec_mergeh (v_bSh, v_bSh);
    
  vec_st (v_bSv, 0, (vector unsigned char *) mv_const);
  vec_st (v_bSh, 0,(vector unsigned char *) mv_const);
}


int main(int argc, char **argv)
{
    char toto[32] __attribute__((aligned(16)));

    foo(toto, toto, 0, 0);
    return 0;
}
