/* Test builtins for MIPS MSA ASE instructions */
/* { dg-do compile } */
/* { dg-options "-mfp64 -mhard-float -mmsa" } */

/* { dg-final { scan-assembler-times "msa_addv_b:.*addv\\.b.*msa_addv_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_addv_h:.*addv\\.h.*msa_addv_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_addv_w:.*addv\\.w.*msa_addv_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_addv_d:.*addv\\.d.*msa_addv_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_addvi_b:.*addvi\\.b.*msa_addvi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_addvi_h:.*addvi\\.h.*msa_addvi_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_addvi_w:.*addvi\\.w.*msa_addvi_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_addvi_d:.*addvi\\.d.*msa_addvi_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_add_a_b:.*add_a\\.b.*msa_add_a_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_add_a_h:.*add_a\\.h.*msa_add_a_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_add_a_w:.*add_a\\.w.*msa_add_a_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_add_a_d:.*add_a\\.d.*msa_add_a_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_a_b:.*adds_a\\.b.*msa_adds_a_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_a_h:.*adds_a\\.h.*msa_adds_a_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_a_w:.*adds_a\\.w.*msa_adds_a_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_a_d:.*adds_a\\.d.*msa_adds_a_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_s_b:.*adds_s\\.b.*msa_adds_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_s_h:.*adds_s\\.h.*msa_adds_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_s_w:.*adds_s\\.w.*msa_adds_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_s_d:.*adds_s\\.d.*msa_adds_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_u_b:.*adds_u\\.b.*msa_adds_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_u_h:.*adds_u\\.h.*msa_adds_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_u_w:.*adds_u\\.w.*msa_adds_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_adds_u_d:.*adds_u\\.d.*msa_adds_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_hadd_s_h:.*hadd_s\\.h.*msa_hadd_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_hadd_s_w:.*hadd_s\\.w.*msa_hadd_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_hadd_s_d:.*hadd_s\\.d.*msa_hadd_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_hadd_u_h:.*hadd_u\\.h.*msa_hadd_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_hadd_u_w:.*hadd_u\\.w.*msa_hadd_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_hadd_u_d:.*hadd_u\\.d.*msa_hadd_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_s_b:.*asub_s\\.b.*msa_asub_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_s_h:.*asub_s\\.h.*msa_asub_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_s_w:.*asub_s\\.w.*msa_asub_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_s_d:.*asub_s\\.d.*msa_asub_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_u_b:.*asub_u\\.b.*msa_asub_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_u_h:.*asub_u\\.h.*msa_asub_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_u_w:.*asub_u\\.w.*msa_asub_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_asub_u_d:.*asub_u\\.d.*msa_asub_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_s_b:.*ave_s\\.b.*msa_ave_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_s_h:.*ave_s\\.h.*msa_ave_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_s_w:.*ave_s\\.w.*msa_ave_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_s_d:.*ave_s\\.d.*msa_ave_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_u_b:.*ave_u\\.b.*msa_ave_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_u_h:.*ave_u\\.h.*msa_ave_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_u_w:.*ave_u\\.w.*msa_ave_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ave_u_d:.*ave_u\\.d.*msa_ave_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_s_b:.*aver_s\\.b.*msa_aver_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_s_h:.*aver_s\\.h.*msa_aver_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_s_w:.*aver_s\\.w.*msa_aver_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_s_d:.*aver_s\\.d.*msa_aver_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_u_b:.*aver_u\\.b.*msa_aver_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_u_h:.*aver_u\\.h.*msa_aver_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_u_w:.*aver_u\\.w.*msa_aver_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_aver_u_d:.*aver_u\\.d.*msa_aver_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_dotp_s_h:.*dotp_s\\.h.*msa_dotp_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_dotp_s_w:.*dotp_s\\.w.*msa_dotp_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_dotp_s_d:.*dotp_s\\.d.*msa_dotp_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_dotp_u_h:.*dotp_u\\.h.*msa_dotp_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_dotp_u_w:.*dotp_u\\.w.*msa_dotp_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_dotp_u_d:.*dotp_u\\.d.*msa_dotp_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpadd_s_h:.*dpadd_s\\.h.*msa_dpadd_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpadd_s_w:.*dpadd_s\\.w.*msa_dpadd_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpadd_s_d:.*dpadd_s\\.d.*msa_dpadd_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpadd_u_h:.*dpadd_u\\.h.*msa_dpadd_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpadd_u_w:.*dpadd_u\\.w.*msa_dpadd_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpadd_u_d:.*dpadd_u\\.d.*msa_dpadd_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpsub_s_h:.*dpsub_s\\.h.*msa_dpsub_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpsub_s_w:.*dpsub_s\\.w.*msa_dpsub_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpsub_s_d:.*dpsub_s\\.d.*msa_dpsub_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpsub_u_h:.*dpsub_u\\.h.*msa_dpsub_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpsub_u_w:.*dpsub_u\\.w.*msa_dpsub_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_dpsub_u_d:.*dpsub_u\\.d.*msa_dpsub_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_s_b:.*div_s\\.b.*msa_div_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_s_h:.*div_s\\.h.*msa_div_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_s_w:.*div_s\\.w.*msa_div_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_s_d:.*div_s\\.d.*msa_div_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_u_b:.*div_u\\.b.*msa_div_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_u_h:.*div_u\\.h.*msa_div_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_u_w:.*div_u\\.w.*msa_div_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_div_u_d:.*div_u\\.d.*msa_div_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_maddv_b:.*maddv\\.b.*msa_maddv_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_maddv_h:.*maddv\\.h.*msa_maddv_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_maddv_w:.*maddv\\.w.*msa_maddv_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_maddv_d:.*maddv\\.d.*msa_maddv_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_a_b:.*max_a\\.b.*msa_max_a_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_a_h:.*max_a\\.h.*msa_max_a_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_a_w:.*max_a\\.w.*msa_max_a_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_a_d:.*max_a\\.d.*msa_max_a_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_a_b:.*min_a\\.b.*msa_min_a_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_a_h:.*min_a\\.h.*msa_min_a_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_a_w:.*min_a\\.w.*msa_min_a_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_a_d:.*min_a\\.d.*msa_min_a_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_s_b:.*max_s\\.b.*msa_max_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_s_h:.*max_s\\.h.*msa_max_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_s_w:.*max_s\\.w.*msa_max_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_s_d:.*max_s\\.d.*msa_max_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_s_b:.*maxi_s\\.b.*msa_maxi_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_s_h:.*maxi_s\\.h.*msa_maxi_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_s_w:.*maxi_s\\.w.*msa_maxi_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_s_d:.*maxi_s\\.d.*msa_maxi_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_u_b:.*max_u\\.b.*msa_max_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_u_h:.*max_u\\.h.*msa_max_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_u_w:.*max_u\\.w.*msa_max_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_max_u_d:.*max_u\\.d.*msa_max_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_u_b:.*maxi_u\\.b.*msa_maxi_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_u_h:.*maxi_u\\.h.*msa_maxi_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_u_w:.*maxi_u\\.w.*msa_maxi_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_maxi_u_d:.*maxi_u\\.d.*msa_maxi_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_s_b:.*min_s\\.b.*msa_min_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_s_h:.*min_s\\.h.*msa_min_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_s_w:.*min_s\\.w.*msa_min_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_s_d:.*min_s\\.d.*msa_min_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_s_b:.*mini_s\\.b.*msa_mini_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_s_h:.*mini_s\\.h.*msa_mini_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_s_w:.*mini_s\\.w.*msa_mini_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_s_d:.*mini_s\\.d.*msa_mini_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_u_b:.*min_u\\.b.*msa_min_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_u_h:.*min_u\\.h.*msa_min_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_u_w:.*min_u\\.w.*msa_min_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_min_u_d:.*min_u\\.d.*msa_min_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_u_b:.*mini_u\\.b.*msa_mini_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_u_h:.*mini_u\\.h.*msa_mini_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_u_w:.*mini_u\\.w.*msa_mini_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mini_u_d:.*mini_u\\.d.*msa_mini_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_msubv_b:.*msubv\\.b.*msa_msubv_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_msubv_h:.*msubv\\.h.*msa_msubv_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_msubv_w:.*msubv\\.w.*msa_msubv_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_msubv_d:.*msubv\\.d.*msa_msubv_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_mulv_b:.*mulv\\.b.*msa_mulv_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_mulv_h:.*mulv\\.h.*msa_mulv_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mulv_w:.*mulv\\.w.*msa_mulv_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mulv_d:.*mulv\\.d.*msa_mulv_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_s_b:.*mod_s\\.b.*msa_mod_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_s_h:.*mod_s\\.h.*msa_mod_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_s_w:.*mod_s\\.w.*msa_mod_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_s_d:.*mod_s\\.d.*msa_mod_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_u_b:.*mod_u\\.b.*msa_mod_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_u_h:.*mod_u\\.h.*msa_mod_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_u_w:.*mod_u\\.w.*msa_mod_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mod_u_d:.*mod_u\\.d.*msa_mod_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_s_b:.*sat_s\\.b.*msa_sat_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_s_h:.*sat_s\\.h.*msa_sat_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_s_w:.*sat_s\\.w.*msa_sat_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_s_d:.*sat_s\\.d.*msa_sat_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_u_b:.*sat_u\\.b.*msa_sat_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_u_h:.*sat_u\\.h.*msa_sat_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_u_w:.*sat_u\\.w.*msa_sat_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sat_u_d:.*sat_u\\.d.*msa_sat_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_s_b:.*subs_s\\.b.*msa_subs_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_s_h:.*subs_s\\.h.*msa_subs_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_s_w:.*subs_s\\.w.*msa_subs_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_s_d:.*subs_s\\.d.*msa_subs_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_u_b:.*subs_u\\.b.*msa_subs_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_u_h:.*subs_u\\.h.*msa_subs_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_u_w:.*subs_u\\.w.*msa_subs_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_subs_u_d:.*subs_u\\.d.*msa_subs_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_hsub_s_h:.*hsub_s\\.h.*msa_hsub_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_hsub_s_w:.*hsub_s\\.w.*msa_hsub_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_hsub_s_d:.*hsub_s\\.d.*msa_hsub_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_hsub_u_h:.*hsub_u\\.h.*msa_hsub_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_hsub_u_w:.*hsub_u\\.w.*msa_hsub_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_hsub_u_d:.*hsub_u\\.d.*msa_hsub_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsuu_s_b:.*subsuu_s\\.b.*msa_subsuu_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsuu_s_h:.*subsuu_s\\.h.*msa_subsuu_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsuu_s_w:.*subsuu_s\\.w.*msa_subsuu_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsuu_s_d:.*subsuu_s\\.d.*msa_subsuu_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsus_u_b:.*subsus_u\\.b.*msa_subsus_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsus_u_h:.*subsus_u\\.h.*msa_subsus_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsus_u_w:.*subsus_u\\.w.*msa_subsus_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_subsus_u_d:.*subsus_u\\.d.*msa_subsus_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_subv_b:.*subv\\.b.*msa_subv_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_subv_h:.*subv\\.h.*msa_subv_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_subv_w:.*subv\\.w.*msa_subv_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_subv_d:.*subv\\.d.*msa_subv_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_subvi_b:.*subvi\\.b.*msa_subvi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_subvi_h:.*subvi\\.h.*msa_subvi_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_subvi_w:.*subvi\\.w.*msa_subvi_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_subvi_d:.*subvi\\.d.*msa_subvi_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_and_v:.*and\\.v.*msa_and_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_andi_b:.*andi\\.b.*msa_andi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclr_b:.*bclr\\.b.*msa_bclr_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclr_h:.*bclr\\.h.*msa_bclr_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclr_w:.*bclr\\.w.*msa_bclr_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclr_d:.*bclr\\.d.*msa_bclr_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclri_b:.*bclri\\.b.*msa_bclri_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclri_h:.*bclri\\.h.*msa_bclri_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclri_w:.*bclri\\.w.*msa_bclri_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bclri_d:.*bclri\\.d.*msa_bclri_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsl_b:.*binsl\\.b.*msa_binsl_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsl_h:.*binsl\\.h.*msa_binsl_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsl_w:.*binsl\\.w.*msa_binsl_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsl_d:.*binsl\\.d.*msa_binsl_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsli_b:.*binsli\\.b.*msa_binsli_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsli_h:.*binsli\\.h.*msa_binsli_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsli_w:.*binsli\\.w.*msa_binsli_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsli_d:.*binsli\\.d.*msa_binsli_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsr_b:.*binsr\\.b.*msa_binsr_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsr_h:.*binsr\\.h.*msa_binsr_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsr_w:.*binsr\\.w.*msa_binsr_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsr_d:.*binsr\\.d.*msa_binsr_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsri_b:.*binsri\\.b.*msa_binsri_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsri_h:.*binsri\\.h.*msa_binsri_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsri_w:.*binsri\\.w.*msa_binsri_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_binsri_d:.*binsri\\.d.*msa_binsri_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bmnz_v:.*bmnz\\.v.*msa_bmnz_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_bmnzi_b:.*bmnzi\\.b.*msa_bmnzi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bmz_v:.*bmz\\.v.*msa_bmz_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_bmzi_b:.*bmzi\\.b.*msa_bmzi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bneg_b:.*bneg\\.b.*msa_bneg_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bneg_h:.*bneg\\.h.*msa_bneg_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bneg_w:.*bneg\\.w.*msa_bneg_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bneg_d:.*bneg\\.d.*msa_bneg_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnegi_b:.*bnegi\\.b.*msa_bnegi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnegi_h:.*bnegi\\.h.*msa_bnegi_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnegi_w:.*bnegi\\.w.*msa_bnegi_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnegi_d:.*bnegi\\.d.*msa_bnegi_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bsel_v:.*bsel\\.v.*msa_bsel_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_bseli_b:.*bseli\\.b.*msa_bseli_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bset_b:.*bset\\.b.*msa_bset_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bset_h:.*bset\\.h.*msa_bset_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bset_w:.*bset\\.w.*msa_bset_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bset_d:.*bset\\.d.*msa_bset_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bseti_b:.*bseti\\.b.*msa_bseti_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bseti_h:.*bseti\\.h.*msa_bseti_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bseti_w:.*bseti\\.w.*msa_bseti_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bseti_d:.*bseti\\.d.*msa_bseti_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_nloc_b:.*nloc\\.b.*msa_nloc_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_nloc_h:.*nloc\\.h.*msa_nloc_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_nloc_w:.*nloc\\.w.*msa_nloc_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_nloc_d:.*nloc\\.d.*msa_nloc_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_nlzc_b:.*nlzc\\.b.*msa_nlzc_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_nlzc_h:.*nlzc\\.h.*msa_nlzc_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_nlzc_w:.*nlzc\\.w.*msa_nlzc_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_nlzc_d:.*nlzc\\.d.*msa_nlzc_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_nor_v:.*nor\\.v.*msa_nor_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_nori_b:.*nori\\.b.*msa_nori_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_pcnt_b:.*pcnt\\.b.*msa_pcnt_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_pcnt_h:.*pcnt\\.h.*msa_pcnt_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_pcnt_w:.*pcnt\\.w.*msa_pcnt_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_pcnt_d:.*pcnt\\.d.*msa_pcnt_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_or_v:.*or\\.v.*msa_or_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_ori_b:.*ori\\.b.*msa_ori_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_xor_v:.*xor\\.v.*msa_xor_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_xori_b:.*xori\\.b.*msa_xori_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sll_b:.*sll\\.b.*msa_sll_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sll_h:.*sll\\.h.*msa_sll_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_sll_w:.*sll\\.w.*msa_sll_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sll_d:.*sll\\.d.*msa_sll_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_slli_b:.*slli\\.b.*msa_slli_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_slli_h:.*slli\\.h.*msa_slli_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_slli_w:.*slli\\.w.*msa_slli_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_slli_d:.*slli\\.d.*msa_slli_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_sra_b:.*sra\\.b.*msa_sra_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sra_h:.*sra\\.h.*msa_sra_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_sra_w:.*sra\\.w.*msa_sra_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sra_d:.*sra\\.d.*msa_sra_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srai_b:.*srai\\.b.*msa_srai_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srai_h:.*srai\\.h.*msa_srai_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srai_w:.*srai\\.w.*msa_srai_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srai_d:.*srai\\.d.*msa_srai_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srar_b:.*srar\\.b.*msa_srar_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srar_h:.*srar\\.h.*msa_srar_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srar_w:.*srar\\.w.*msa_srar_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srar_d:.*srar\\.d.*msa_srar_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srari_b:.*srari\\.b.*msa_srari_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srari_h:.*srari\\.h.*msa_srari_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srari_w:.*srari\\.w.*msa_srari_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srari_d:.*srari\\.d.*msa_srari_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srl_b:.*srl\\.b.*msa_srl_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srl_h:.*srl\\.h.*msa_srl_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srl_w:.*srl\\.w.*msa_srl_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srl_d:.*srl\\.d.*msa_srl_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srli_b:.*srli\\.b.*msa_srli_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srli_h:.*srli\\.h.*msa_srli_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srli_w:.*srli\\.w.*msa_srli_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srli_d:.*srli\\.d.*msa_srli_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlr_b:.*srlr\\.b.*msa_srlr_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlr_h:.*srlr\\.h.*msa_srlr_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlr_w:.*srlr\\.w.*msa_srlr_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlr_d:.*srlr\\.d.*msa_srlr_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlri_b:.*srlri\\.b.*msa_srlri_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlri_h:.*srlri\\.h.*msa_srlri_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlri_w:.*srlri\\.w.*msa_srlri_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_srlri_d:.*srlri\\.d.*msa_srlri_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fadd_w:.*fadd\\.w.*msa_fadd_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fadd_d:.*fadd\\.d.*msa_fadd_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fdiv_w:.*fdiv\\.w.*msa_fdiv_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fdiv_d:.*fdiv\\.d.*msa_fdiv_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexp2_w:.*fexp2\\.w.*msa_fexp2_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexp2_d:.*fexp2\\.d.*msa_fexp2_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_flog2_w:.*flog2\\.w.*msa_flog2_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_flog2_d:.*flog2\\.d.*msa_flog2_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmadd_w:.*fmadd\\.w.*msa_fmadd_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmadd_d:.*fmadd\\.d.*msa_fmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmsub_w:.*fmsub\\.w.*msa_fmsub_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmsub_d:.*fmsub\\.d.*msa_fmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmax_w:.*fmax\\.w.*msa_fmax_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmax_d:.*fmax\\.d.*msa_fmax_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmin_w:.*fmin\\.w.*msa_fmin_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmin_d:.*fmin\\.d.*msa_fmin_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmax_a_w:.*fmax_a\\.w.*msa_fmax_a_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmax_a_d:.*fmax_a\\.d.*msa_fmax_a_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmin_a_w:.*fmin_a\\.w.*msa_fmin_a_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmin_a_d:.*fmin_a\\.d.*msa_fmin_a_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmul_w:.*fmul\\.w.*msa_fmul_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fmul_d:.*fmul\\.d.*msa_fmul_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_frcp_w:.*frcp\\.w.*msa_frcp_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_frcp_d:.*frcp\\.d.*msa_frcp_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_frint_w:.*frint\\.w.*msa_frint_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_frint_d:.*frint\\.d.*msa_frint_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_frsqrt_w:.*frsqrt\\.w.*msa_frsqrt_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_frsqrt_d:.*frsqrt\\.d.*msa_frsqrt_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsqrt_w:.*fsqrt\\.w.*msa_fsqrt_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsqrt_d:.*fsqrt\\.d.*msa_fsqrt_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsub_w:.*fsub\\.w.*msa_fsub_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsub_d:.*fsub\\.d.*msa_fsub_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fclass_w:.*fclass\\.w.*msa_fclass_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fclass_d:.*fclass\\.d.*msa_fclass_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcaf_w:.*fcaf\\.w.*msa_fcaf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcaf_d:.*fcaf\\.d.*msa_fcaf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcun_w:.*fcun\\.w.*msa_fcun_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcun_d:.*fcun\\.d.*msa_fcun_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcor_w:.*fcor\\.w.*msa_fcor_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcor_d:.*fcor\\.d.*msa_fcor_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fceq_w:.*fceq\\.w.*msa_fceq_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fceq_d:.*fceq\\.d.*msa_fceq_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcune_w:.*fcune\\.w.*msa_fcune_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcune_d:.*fcune\\.d.*msa_fcune_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcueq_w:.*fcueq\\.w.*msa_fcueq_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcueq_d:.*fcueq\\.d.*msa_fcueq_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcne_w:.*fcne\\.w.*msa_fcne_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcne_d:.*fcne\\.d.*msa_fcne_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fclt_w:.*fclt\\.w.*msa_fclt_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fclt_d:.*fclt\\.d.*msa_fclt_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcult_w:.*fcult\\.w.*msa_fcult_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcult_d:.*fcult\\.d.*msa_fcult_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcle_w:.*fcle\\.w.*msa_fcle_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcle_d:.*fcle\\.d.*msa_fcle_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcule_w:.*fcule\\.w.*msa_fcule_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fcule_d:.*fcule\\.d.*msa_fcule_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsaf_w:.*fsaf\\.w.*msa_fsaf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsaf_d:.*fsaf\\.d.*msa_fsaf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsun_w:.*fsun\\.w.*msa_fsun_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsun_d:.*fsun\\.d.*msa_fsun_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsor_w:.*fsor\\.w.*msa_fsor_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsor_d:.*fsor\\.d.*msa_fsor_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fseq_w:.*fseq\\.w.*msa_fseq_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fseq_d:.*fseq\\.d.*msa_fseq_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsune_w:.*fsune\\.w.*msa_fsune_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsune_d:.*fsune\\.d.*msa_fsune_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsueq_w:.*fsueq\\.w.*msa_fsueq_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsueq_d:.*fsueq\\.d.*msa_fsueq_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsne_w:.*fsne\\.w.*msa_fsne_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsne_d:.*fsne\\.d.*msa_fsne_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fslt_w:.*fslt\\.w.*msa_fslt_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fslt_d:.*fslt\\.d.*msa_fslt_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsult_w:.*fsult\\.w.*msa_fsult_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsult_d:.*fsult\\.d.*msa_fsult_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsle_w:.*fsle\\.w.*msa_fsle_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsle_d:.*fsle\\.d.*msa_fsle_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsule_w:.*fsule\\.w.*msa_fsule_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fsule_d:.*fsule\\.d.*msa_fsule_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexupl_w:.*fexupl\\.w.*msa_fexupl_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexupl_d:.*fexupl\\.d.*msa_fexupl_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexupr_w:.*fexupr\\.w.*msa_fexupr_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexupr_d:.*fexupr\\.d.*msa_fexupr_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexdo_h:.*fexdo\\.h.*msa_fexdo_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_fexdo_w:.*fexdo\\.w.*msa_fexdo_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffint_s_w:.*ffint_s\\.w.*msa_ffint_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffint_s_d:.*ffint_s\\.d.*msa_ffint_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffint_u_w:.*ffint_u\\.w.*msa_ffint_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffint_u_d:.*ffint_u\\.d.*msa_ffint_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffql_w:.*ffql\\.w.*msa_ffql_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffql_d:.*ffql\\.d.*msa_ffql_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffqr_w:.*ffqr\\.w.*msa_ffqr_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ffqr_d:.*ffqr\\.d.*msa_ffqr_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftint_s_w:.*ftint_s\\.w.*msa_ftint_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftint_s_d:.*ftint_s\\.d.*msa_ftint_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftint_u_w:.*ftint_u\\.w.*msa_ftint_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftint_u_d:.*ftint_u\\.d.*msa_ftint_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftrunc_s_w:.*ftrunc_s\\.w.*msa_ftrunc_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftrunc_s_d:.*ftrunc_s\\.d.*msa_ftrunc_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftrunc_u_w:.*ftrunc_u\\.w.*msa_ftrunc_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftrunc_u_d:.*ftrunc_u\\.d.*msa_ftrunc_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftq_h:.*ftq\\.h.*msa_ftq_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ftq_w:.*ftq\\.w.*msa_ftq_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_madd_q_h:.*madd_q\\.h.*msa_madd_q_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_madd_q_w:.*madd_q\\.w.*msa_madd_q_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_maddr_q_h:.*maddr_q\\.h.*msa_maddr_q_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_maddr_q_w:.*maddr_q\\.w.*msa_maddr_q_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_msub_q_h:.*msub_q\\.h.*msa_msub_q_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_msub_q_w:.*msub_q\\.w.*msa_msub_q_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_msubr_q_h:.*msubr_q\\.h.*msa_msubr_q_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_msubr_q_w:.*msubr_q\\.w.*msa_msubr_q_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mul_q_h:.*mul_q\\.h.*msa_mul_q_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mul_q_w:.*mul_q\\.w.*msa_mul_q_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_mulr_q_h:.*mulr_q\\.h.*msa_mulr_q_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_mulr_q_w:.*mulr_q\\.w.*msa_mulr_q_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceq_b:.*ceq\\.b.*msa_ceq_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceq_h:.*ceq\\.h.*msa_ceq_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceq_w:.*ceq\\.w.*msa_ceq_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceq_d:.*ceq\\.d.*msa_ceq_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceqi_b:.*ceqi\\.b.*msa_ceqi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceqi_h:.*ceqi\\.h.*msa_ceqi_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceqi_w:.*ceqi\\.w.*msa_ceqi_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ceqi_d:.*ceqi\\.d.*msa_ceqi_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_s_b:.*cle_s\\.b.*msa_cle_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_s_h:.*cle_s\\.h.*msa_cle_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_s_w:.*cle_s\\.w.*msa_cle_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_s_d:.*cle_s\\.d.*msa_cle_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_s_b:.*clei_s\\.b.*msa_clei_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_s_h:.*clei_s\\.h.*msa_clei_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_s_w:.*clei_s\\.w.*msa_clei_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_s_d:.*clei_s\\.d.*msa_clei_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_u_b:.*cle_u\\.b.*msa_cle_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_u_h:.*cle_u\\.h.*msa_cle_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_u_w:.*cle_u\\.w.*msa_cle_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_cle_u_d:.*cle_u\\.d.*msa_cle_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_u_b:.*clei_u\\.b.*msa_clei_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_u_h:.*clei_u\\.h.*msa_clei_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_u_w:.*clei_u\\.w.*msa_clei_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_clei_u_d:.*clei_u\\.d.*msa_clei_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_s_b:.*clt_s\\.b.*msa_clt_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_s_h:.*clt_s\\.h.*msa_clt_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_s_w:.*clt_s\\.w.*msa_clt_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_s_d:.*clt_s\\.d.*msa_clt_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_s_b:.*clti_s\\.b.*msa_clti_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_s_h:.*clti_s\\.h.*msa_clti_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_s_w:.*clti_s\\.w.*msa_clti_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_s_d:.*clti_s\\.d.*msa_clti_s_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_u_b:.*clt_u\\.b.*msa_clt_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_u_h:.*clt_u\\.h.*msa_clt_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_u_w:.*clt_u\\.w.*msa_clt_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_clt_u_d:.*clt_u\\.d.*msa_clt_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_u_b:.*clti_u\\.b.*msa_clti_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_u_h:.*clti_u\\.h.*msa_clti_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_u_w:.*clti_u\\.w.*msa_clti_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_clti_u_d:.*clti_u\\.d.*msa_clti_u_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnz_v:.*li\t\\\$\\d+,1.*bnz\\.v.*msa_bnz_v|msa_bnz_v:.*bz\\.v.*li\t\\\$\\d+,1.*msa_bnz_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_bz_v:.*li\t\\\$\\d+,1.*bz\\.v.*msa_bz_v|msa_bz_v:.*bnz\\.v.*li\t\\\$\\d+,1.*msa_bz_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnz_b:.*li\t\\\$\\d+,1.*bnz\\.b.*msa_bnz_b|msa_bnz_b:.*bz\\.b.*li\t\\\$\\d+,1.*msa_bnz_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnz_h:.*li\t\\\$\\d+,1.*bnz\\.h.*msa_bnz_h|msa_bnz_h:.*bz\\.h.*li\t\\\$\\d+,1.*msa_bnz_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnz_w:.*li\t\\\$\\d+,1.*bnz\\.w.*msa_bnz_w|msa_bnz_w:.*bz\\.w.*li\t\\\$\\d+,1.*msa_bnz_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bnz_d:.*li\t\\\$\\d+,1.*bnz\\.d.*msa_bnz_d|msa_bnz_d:.*bz\\.d.*li\t\\\$\\d+,1.*msa_bnz_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_bz_b:.*li\t\\\$\\d+,1.*bz\\.b.*msa_bz_b|msa_bz_b:.*bnz\\.b.*li\t\\\$\\d+,1.*msa_bz_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_bz_h:.*li\t\\\$\\d+,1.*bz\\.h.*msa_bz_h|msa_bz_h:.*bnz\\.h.*li\t\\\$\\d+,1.*msa_bz_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_bz_w:.*li\t\\\$\\d+,1.*bz\\.w.*msa_bz_w|msa_bz_w:.*bnz\\.w.*li\t\\\$\\d+,1.*msa_bz_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_bz_d:.*li\t\\\$\\d+,1.*bz\\.d.*msa_bz_d|msa_bz_d:.*bnz\\.d.*li\t\\\$\\d+,1.*msa_bz_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_cfcmsa:.*cfcmsa.*msa_cfcmsa" 1 } } */
/* { dg-final { scan-assembler-times "msa_ctcmsa:.*ctcmsa.*msa_ctcmsa" 1 } } */
/* { dg-final { scan-assembler-times "msa_ld_b:.*ld\\.b.*msa_ld_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ld_h:.*ld\\.h.*msa_ld_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ld_w:.*ld\\.w.*msa_ld_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ld_d:.*ld\\.d.*msa_ld_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ldi_b:.*ldi\\.b.*msa_ldi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ldi_h:.*ldi\\.h.*msa_ldi_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ldi_w:.*ldi\\.w.*msa_ldi_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ldi_d:.*ldi\\.d.*msa_ldi_d" 1 } } */
/* Note: move.v is likely to be optimised out.  */
/* { dg-final { scan-assembler-times "msa_move_v:.*msa_move_v|msa_move_v:.*move\\.v.*msa_move_v" 1 } } */
/* { dg-final { scan-assembler-times "msa_splat_b:.*splat\\.b.*msa_splat_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_splat_h:.*splat\\.h.*msa_splat_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_splat_w:.*splat\\.w.*msa_splat_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_splat_d:.*splat\\.d.*msa_splat_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_splati_b:.*splati\\.b.*msa_splati_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_splati_h:.*splati\\.h.*msa_splati_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_splati_w:.*splati\\.w.*msa_splati_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_splati_d:.*splati\\.d.*msa_splati_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_fill_b:.*fill\\.b.*msa_fill_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_fill_h:.*fill\\.h.*msa_fill_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_fill_w:.*fill\\.w.*msa_fill_w" 1 } } */
/* Note: some instructions are only available on MIPS64, thus, these will be
   replaced with equivalent ones on MIPS32.  */
/* { dg-final { scan-assembler-times "msa_fill_d:.*fill\\.d.*msa_fill_d" 1 { target mips64 } } } */
/* { dg-final { scan-assembler-times "msa_fill_d:.*fill\\.w.*insert.w.*insert.w.*msa_fill_d" 1 { target {! mips64 } } } } */
/* { dg-final { scan-assembler-times "msa_insert_b:.*insert\\.b.*msa_insert_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_insert_h:.*insert\\.h.*msa_insert_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_insert_w:.*insert\\.w.*msa_insert_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_insert_d:.*insert\\.d.*msa_insert_d" 1 { target mips64 } } } */
/* { dg-final { scan-assembler "msa_insert_d:.*(sra.*insert.w.*insert.w|insert.w.*sra.*insert.w).*msa_insert_d" { target {! mips64 } } } } */
/* { dg-final { scan-assembler-times "msa_insve_b:.*insve\\.b.*msa_insve_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_insve_h:.*insve\\.h.*msa_insve_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_insve_w:.*insve\\.w.*msa_insve_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_insve_d:.*insve\\.d.*msa_insve_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_s_b:.*copy_s\\.b.*msa_copy_s_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_s_h:.*copy_s\\.h.*msa_copy_s_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_s_w:.*copy_s\\.w.*msa_copy_s_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_s_d:.*copy_s\\.d.*msa_copy_s_d" 1 { target mips64 } } } */
/* { dg-final { scan-assembler-times "msa_copy_s_d:.*copy_s\\.w.*copy_s\\.w.*msa_copy_s_d" 1 { target {! mips64 } } } } */
/* { dg-final { scan-assembler-times "msa_copy_u_b:.*copy_u\\.b.*msa_copy_u_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_u_h:.*copy_u\\.h.*msa_copy_u_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_u_w:.*copy_s\\.w.*msa_copy_u_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_copy_u_d:.*copy_s\\.d.*msa_copy_u_d" 1 { target mips64 } } } */
/* { dg-final { scan-assembler-times "msa_copy_u_d:.*copy_s\\.w.*copy_s\\.w.*msa_copy_u_d" 1 { target {! mips64 } } } } */
/* { dg-final { scan-assembler-times "msa_st_b:.*st\\.b.*msa_st_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_st_h:.*st\\.h.*msa_st_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_st_w:.*st\\.w.*msa_st_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_st_d:.*st\\.d.*msa_st_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvev_b:.*ilvev\\.b.*msa_ilvev_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvev_h:.*ilvev\\.h.*msa_ilvev_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvev_w:.*ilvev\\.w.*msa_ilvev_w" 1 } } */
/* Note: ilvev.d is equivalent to ilvr.d.  */
/* { dg-final { scan-assembler-times "msa_ilvev_d:.*ilvev\\.d.*msa_ilvev_d|msa_ilvev_d:.*ilvr\\.d.*msa_ilvev_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvod_b:.*ilvod\\.b.*msa_ilvod_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvod_h:.*ilvod\\.h.*msa_ilvod_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvod_w:.*ilvod\\.w.*msa_ilvod_w" 1 } } */
/* Note: ilvod.d is equivalent to ilvl.d.  */
/* { dg-final { scan-assembler-times "msa_ilvod_d:.*ilvod\\.d.*msa_ilvod_d|msa_ilvod_d:.*ilvl\\.d.*msa_ilvod_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvl_b:.*ilvl\\.b.*msa_ilvl_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvl_h:.*ilvl\\.h.*msa_ilvl_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvl_w:.*ilvl\\.w.*msa_ilvl_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvl_d:.*ilvl\\.d.*msa_ilvl_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvr_b:.*ilvr\\.b.*msa_ilvr_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvr_h:.*ilvr\\.h.*msa_ilvr_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvr_w:.*ilvr\\.w.*msa_ilvr_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_ilvr_d:.*ilvr\\.d.*msa_ilvr_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_pckev_b:.*pckev\\.b.*msa_pckev_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_pckev_h:.*pckev\\.h.*msa_pckev_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_pckev_w:.*pckev\\.w.*msa_pckev_w" 1 } } */
/* Note: ilvr.d is equivalent to pckev.d.  */
/* { dg-final { scan-assembler-times "msa_pckev_d:.*ilvr\\.d.*msa_pckev_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_pckod_b:.*pckod\\.b.*msa_pckod_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_pckod_h:.*pckod\\.h.*msa_pckod_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_pckod_w:.*pckod\\.w.*msa_pckod_w" 1 } } */
/* Note: ilvl.d is equivalent to pckod.d.  */
/* { dg-final { scan-assembler-times "msa_pckod_d:.*ilvl\\.d.*msa_pckod_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_shf_b:.*shf\\.b.*msa_shf_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_shf_h:.*shf\\.h.*msa_shf_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_shf_w:.*shf\\.w.*msa_shf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sld_b:.*sld\\.b.*msa_sld_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sld_h:.*sld\\.h.*msa_sld_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_sld_w:.*sld\\.w.*msa_sld_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sld_d:.*sld\\.d.*msa_sld_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_sldi_b:.*sldi\\.b.*msa_sldi_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_sldi_h:.*sldi\\.h.*msa_sldi_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_sldi_w:.*sldi\\.w.*msa_sldi_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_sldi_d:.*sldi\\.d.*msa_sldi_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_vshf_b:.*vshf\\.b.*msa_vshf_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_vshf_h:.*vshf\\.h.*msa_vshf_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_vshf_w:.*vshf\\.w.*msa_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_vshf_d:.*vshf\\.d.*msa_vshf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_s_vshf_b:.*vshf.b.*msa_gcc_1_s_vshf_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_s_vshf_h:.*vshf.h.*msa_gcc_1_s_vshf_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_s_vshf_w:.*vshf.w.*msa_gcc_1_s_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_s_vshf_d:.*vshf.d.*msa_gcc_1_s_vshf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_u_vshf_b:.*vshf.b.*msa_gcc_1_u_vshf_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_u_vshf_h:.*vshf.h.*msa_gcc_1_u_vshf_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_u_vshf_w:.*vshf.w.*msa_gcc_1_u_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_1_u_vshf_d:.*vshf.d.*msa_gcc_1_u_vshf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_s_vshf_b:.*vshf.b.*msa_gcc_2_s_vshf_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_s_vshf_h:.*vshf.h.*msa_gcc_2_s_vshf_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_s_vshf_w:.*vshf.w.*msa_gcc_2_s_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_s_vshf_d:.*vshf.d.*msa_gcc_2_s_vshf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_u_vshf_b:.*vshf.b.*msa_gcc_2_u_vshf_b" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_u_vshf_h:.*vshf.h.*msa_gcc_2_u_vshf_h" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_u_vshf_w:.*vshf.w.*msa_gcc_2_u_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_2_u_vshf_d:.*vshf.d.*msa_gcc_2_u_vshf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_3_vshf_w:.*vshf.w.*msa_gcc_3_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_3_vshf_d:.*vshf.d.*msa_gcc_3_vshf_d" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_4_vshf_w:.*vshf.w.*msa_gcc_4_vshf_w" 1 } } */
/* { dg-final { scan-assembler-times "msa_gcc_4_vshf_d:.*vshf.d.*msa_gcc_4_vshf_d" 1 } } */

#include <msa.h>

#define U5MAX 31
#define U8MAX 255
#define S5MAX 15

#define v16i8_DF b
#define v8i16_DF h
#define v4i32_DF w
#define v2i64_DF d
#define v16u8_DF b
#define v8u16_DF h
#define v4u32_DF w
#define v2u64_DF d
#define v4f32_DF w
#define v2f64_DF d

#define v16i8_DBL v8i16
#define v8i16_DBL v4i32
#define v4i32_DBL v2i64
#define v16u8_DBL v8u16
#define v8u16_DBL v4u32
#define v4u32_DBL v2u64

#define v16i8_DDF h
#define v8i16_DDF w
#define v4i32_DDF d
#define v16u8_DDF h
#define v8u16_DDF w
#define v4u32_DDF d

#define v4f32_HDF h
#define v2f64_HDF w

/* Signed twice the size result.  */
#define v16u8_SDBL v8i16
#define v8u16_SDBL v4i32
#define v4u32_SDBL v2i64

/* Signed values for unsigned type, subsus_u_* instructions.  */
#define v16u8_S v16i8
#define v8u16_S v8i16
#define v4u32_S v4i32
#define v2u64_S v2i64

/* Integer elements for fexp2.  */
#define v4f32_FEXP2 v4i32
#define v2f64_FEXP2 v2i64

/* Return type for floating-point conversion instructions.  */
#define v4f32_FCNV v8i16
#define v2f64_FCNV v4f32
#define v4f32_FSINT v4i32
#define v2f64_FSINT v2i64
#define v4f32_FUINT v4u32
#define v2f64_FUINT v2u64
#define v4f32_FFP v8i16
#define v2f64_FFP v4i32

/* Integer result for floating point operations.  */
#define v4f32_FRES v4i32
#define v2f64_FRES v2i64

/* Return type for compare unsign instructions.  */
#define v16u8_CMP v16i8
#define v8u16_CMP v8i16
#define v4u32_CMP v4i32
#define v2u64_CMP v2i64

#define PASTE_BUILTIN(NAME, DF) __builtin_msa_ ## NAME ## _ ## DF
#define EVAL_BUILTIN(NAME, DF) PASTE_BUILTIN (NAME, DF)
#define BUILTIN(NAME, DF) EVAL_BUILTIN (NAME, DF)

#define FN_EVAL(NAME, T) msa_ ## NAME ## _ ## T
#define FN(NAME, T) FN_EVAL (NAME, T)

/* MSA Arithmetic builtins.  */
#define ADDV(T) NOMIPS16 T FN (addv, T ## _DF) (T i, T j) { return BUILTIN (addv, T ## _DF) (i, j); }
#define ADDVI(T) NOMIPS16 T FN (addvi, T ## _DF) (T i) { return BUILTIN (addvi, T ## _DF) (i, U5MAX); }
#define ADD_A(T) NOMIPS16 T FN (add_a, T ## _DF) (T i, T j) { return BUILTIN (add_a, T ## _DF) (i, j); }
#define ADDS_A(T) NOMIPS16 T FN (adds_a, T ## _DF) (T i, T j) { return BUILTIN (adds_a, T ## _DF) (i, j); }
#define ADDS_S(T) NOMIPS16 T FN (adds_s, T ## _DF) (T i, T j) { return BUILTIN (adds_s, T ## _DF) (i, j); }
#define ADDS_U(T) NOMIPS16 T FN (adds_u, T ## _DF) (T i, T j) { return BUILTIN (adds_u, T ## _DF) (i, j); }
#define HADD_S(T) NOMIPS16 T ## _DBL FN (hadd_s, T ## _DDF) (T i, T j) { return BUILTIN (hadd_s, T ## _DDF) (i, j); }
#define HADD_U(T) NOMIPS16 T ## _DBL FN (hadd_u, T ## _DDF) (T i, T j) { return BUILTIN (hadd_u, T ## _DDF) (i, j); }
#define ASUB_S(T) NOMIPS16 T FN (asub_s, T ## _DF) (T i, T j) { return BUILTIN (asub_s, T ## _DF) (i, j); }
#define ASUB_U(T) NOMIPS16 T FN (asub_u, T ## _DF) (T i, T j) { return BUILTIN (asub_u, T ## _DF) (i, j); }
#define AVE_S(T) NOMIPS16 T FN (ave_s, T ## _DF) (T i, T j) { return BUILTIN (ave_s, T ## _DF) (i, j); }
#define AVE_U(T) NOMIPS16 T FN (ave_u, T ## _DF) (T i, T j) { return BUILTIN (ave_u, T ## _DF) (i, j); }
#define AVER_S(T) NOMIPS16 T FN (aver_s, T ## _DF) (T i, T j) { return BUILTIN (aver_s, T ## _DF) (i, j); }
#define AVER_U(T) NOMIPS16 T FN (aver_u, T ## _DF) (T i, T j) { return BUILTIN (aver_u, T ## _DF) (i, j); }
#define DOTP_S(T) NOMIPS16 T ## _DBL FN (dotp_s, T ## _DDF) (T i, T j) { return BUILTIN (dotp_s, T ## _DDF) (i, j); }
#define DOTP_U(T) NOMIPS16 T ## _DBL FN (dotp_u, T ## _DDF) (T i, T j) { return BUILTIN (dotp_u, T ## _DDF) (i, j); }
#define DPADD_S(T) NOMIPS16 T ## _DBL FN (dpadd_s, T ## _DDF) (T ## _DBL i, T j, T k) { return BUILTIN (dpadd_s, T ## _DDF) (i, j, k); }
#define DPADD_U(T) NOMIPS16 T ## _DBL FN (dpadd_u, T ## _DDF) (T ## _DBL i, T j, T k) { return BUILTIN (dpadd_u, T ## _DDF) (i, j, k); }
#define DPSUB_S(T) NOMIPS16 T ## _DBL FN (dpsub_s, T ## _DDF) (T ## _DBL i, T j, T k) { return BUILTIN (dpsub_s, T ## _DDF) (i, j, k); }
#define DPSUB_U(T) NOMIPS16 T ## _SDBL FN (dpsub_u, T ## _DDF) (T ## _SDBL i, T j, T k) { return BUILTIN (dpsub_u, T ## _DDF) (i, j, k); }
#define DIV_S(T) NOMIPS16 T FN (div_s, T ## _DF) (T i, T j) { return BUILTIN (div_s, T ## _DF) (i, j); }
#define DIV_U(T) NOMIPS16 T FN (div_u, T ## _DF) (T i, T j) { return BUILTIN (div_u, T ## _DF) (i, j); }
#define MADDV(T) NOMIPS16 T FN (maddv, T ## _DF) (T i, T j, T k) { return BUILTIN (maddv, T ## _DF) (i, j, k); }
#define MAX_A(T) NOMIPS16 T FN (max_a, T ## _DF) (T i, T j) { return BUILTIN (max_a, T ## _DF) (i, j); }
#define MIN_A(T) NOMIPS16 T FN (min_a, T ## _DF) (T i, T j) { return BUILTIN (min_a, T ## _DF) (i, j); }
#define MAX_S(T) NOMIPS16 T FN (max_s, T ## _DF) (T i, T j) { return BUILTIN (max_s, T ## _DF) (i, j); }
#define MAXI_S(T) NOMIPS16 T FN (maxi_s, T ## _DF) (T i) { return BUILTIN (maxi_s, T ## _DF) (i, S5MAX); }
#define MAX_U(T) NOMIPS16 T FN (max_u, T ## _DF) (T i, T j) { return BUILTIN (max_u, T ## _DF) (i, j); }
#define MAXI_U(T) NOMIPS16 T FN (maxi_u, T ## _DF) (T i) { return BUILTIN (maxi_u, T ## _DF) (i, S5MAX); }
#define MIN_S(T) NOMIPS16 T FN (min_s, T ## _DF) (T i, T j) { return BUILTIN (min_s, T ## _DF) (i, j); }
#define MINI_S(T) NOMIPS16 T FN (mini_s, T ## _DF) (T i) { return BUILTIN (mini_s, T ## _DF) (i, S5MAX); }
#define MIN_U(T) NOMIPS16 T FN (min_u, T ## _DF) (T i, T j) { return BUILTIN (min_u, T ## _DF) (i, j); }
#define MINI_U(T) NOMIPS16 T FN (mini_u, T ## _DF) (T i) { return BUILTIN (mini_u, T ## _DF) (i, S5MAX); }
#define MSUBV(T) NOMIPS16 T FN (msubv, T ## _DF) (T i, T j, T k) { return BUILTIN (msubv, T ## _DF) (i, j, k); }
#define MULV(T) NOMIPS16 T FN (mulv, T ## _DF) (T i, T j) { return BUILTIN (mulv, T ## _DF) (i, j); }
#define MOD_S(T) NOMIPS16 T FN (mod_s, T ## _DF) (T i, T j) { return BUILTIN (mod_s, T ## _DF) (i, j); }
#define MOD_U(T) NOMIPS16 T FN (mod_u, T ## _DF) (T i, T j) { return BUILTIN (mod_u, T ## _DF) (i, j); }
#define SAT_S(T) NOMIPS16 T FN (sat_s, T ## _DF) (T i) { return BUILTIN (sat_s, T ## _DF) (i, 7); }
#define SAT_U(T) NOMIPS16 T FN (sat_u, T ## _DF) (T i) { return BUILTIN (sat_u, T ## _DF) (i, 7); }
#define SUBS_S(T) NOMIPS16 T FN (subs_s, T ## _DF) (T i, T j) { return BUILTIN (subs_s, T ## _DF) (i, j); }
#define SUBS_U(T) NOMIPS16 T FN (subs_u, T ## _DF) (T i, T j) { return BUILTIN (subs_u, T ## _DF) (i, j); }
#define HSUB_S(T) NOMIPS16 T ## _DBL FN (hsub_s, T ## _DDF) (T i, T j) { return BUILTIN (hsub_s, T ## _DDF) (i, j); }
#define HSUB_U(T) NOMIPS16 T ## _SDBL FN (hsub_u, T ## _DDF) (T i, T j) { return BUILTIN (hsub_u, T ## _DDF) (i, j); }
#define SUBSUU_S(T) NOMIPS16 T ## _S FN (subsuu_s, T ## _DF) (T i, T j) { return BUILTIN (subsuu_s, T ## _DF) (i, j); }
#define SUBSUS_U(T) NOMIPS16 T FN (subsus_u, T ## _DF) (T i, T ## _S j) { return BUILTIN (subsus_u, T ## _DF) (i, j); }
#define SUBV(T) NOMIPS16 T FN (subv, T ## _DF) (T i, T j) { return BUILTIN (subv, T ## _DF) (i, j); }
#define SUBVI(T) NOMIPS16 T FN (subvi, T ## _DF) (T i) { return BUILTIN (subvi, T ## _DF) (i, U5MAX); }

/* MSA Bitwise builtins.  */
#define AND(T) NOMIPS16 T FN (and, v) (T i, T j) { return BUILTIN (and, v) (i, j); }
#define ANDI(T) NOMIPS16 T FN (andi, T ## _DF) (T i) { return BUILTIN (andi, T ## _DF) (i, 252); }
#define BCLR(T) NOMIPS16 T FN (bclr, T ## _DF) (T i, T j) { return BUILTIN (bclr, T ## _DF) (i, j); }
#define BCLRI(T) NOMIPS16 T FN (bclri, T ## _DF) (T i) { return BUILTIN (bclri, T ## _DF) (i, 0); }
#define BINSL(T) NOMIPS16 T FN (binsl, T ## _DF) (T i, T j, T k) { return BUILTIN (binsl, T ## _DF) (i, j, k); }
#define BINSLI(T) NOMIPS16 T FN (binsli, T ## _DF) (T i, T j) { return BUILTIN (binsli, T ## _DF) (i, j, 0); }
#define BINSR(T) NOMIPS16 T FN (binsr, T ## _DF) (T i, T j, T k) { return BUILTIN (binsr, T ## _DF) (i, j, k); }
#define BINSRI(T) NOMIPS16 T FN (binsri, T ## _DF) (T i, T j) { return BUILTIN (binsri, T ## _DF) (i, j, 0); }
#define BMNZ(T) NOMIPS16 T FN (bmnz, v) (T i, T j, T k) { return BUILTIN (bmnz, v) (i, j, k); }
#define BMNZI(T) NOMIPS16 T FN (bmnzi, T ## _DF) (T i, T j) { return BUILTIN (bmnzi, T ## _DF) (i, j, 254); }
#define BMZ(T) NOMIPS16 T FN (bmz, v) (T i, T j, T k) { return BUILTIN (bmz, v) (i, j, k); }
#define BMZI(T) NOMIPS16 T FN (bmzi, T ## _DF) (T i, T j) { return BUILTIN (bmzi, T ## _DF) (i, j, 254); }
#define BNEG(T) NOMIPS16 T FN (bneg, T ## _DF) (T i, T j) { return BUILTIN (bneg, T ## _DF) (i, j); }
#define BNEGI(T) NOMIPS16 T FN (bnegi, T ## _DF) (T i) { return BUILTIN (bnegi, T ## _DF) (i, 0); }
#define BSEL(T) NOMIPS16 T FN (bsel, v) (T i, T j, T k) { return BUILTIN (bsel, v) (i, j, k); }
#define BSELI(T) NOMIPS16 T FN (bseli, T ## _DF) (T i, T j) { return BUILTIN (bseli, T ## _DF) (i, j, U8MAX-1); }
#define BSET(T) NOMIPS16 T FN (bset, T ## _DF) (T i, T j) { return BUILTIN (bset, T ## _DF) (i, j); }
#define BSETI(T) NOMIPS16 T FN (bseti, T ## _DF) (T i) { return BUILTIN (bseti, T ## _DF) (i, 0); }
#define NLOC(T) NOMIPS16 T FN (nloc, T ## _DF) (T i) { return BUILTIN (nloc, T ## _DF) (i); }
#define NLZC(T) NOMIPS16 T FN (nlzc, T ## _DF) (T i) { return BUILTIN (nlzc, T ## _DF) (i); }
#define NOR(T) NOMIPS16 T FN (nor, v) (T i, T j) { return BUILTIN (nor, v) (i, j); }
#define NORI(T) NOMIPS16 T FN (nori, T ## _DF) (T i) { return BUILTIN (nori, T ## _DF) (i, 254); }
#define PCNT(T) NOMIPS16 T FN (pcnt, T ## _DF) (T i) { return BUILTIN (pcnt, T ## _DF) (i); }
#define OR(T) NOMIPS16 T FN (or, v) (T i, T j) { return BUILTIN (or, v) (i, j); }
#define ORI(T) NOMIPS16 T FN (ori, T ## _DF) (T i) { return BUILTIN (ori, T ## _DF) (i, 252); }
#define XOR(T) NOMIPS16 T FN (xor, v) (T i, T j) { return BUILTIN (xor, v) (i, j); }
#define XORI(T) NOMIPS16 T FN (xori, T ## _DF) (T i) { return BUILTIN (xori, T ## _DF) (i, 254); }
#define SLL(T) NOMIPS16 T FN (sll, T ## _DF) (T i, T j) { return BUILTIN (sll, T ## _DF) (i, j); }
#define SLLI(T) NOMIPS16 T FN (slli, T ## _DF) (T i) { return BUILTIN (slli, T ## _DF) (i, 1); }
#define SRA(T) NOMIPS16 T FN (sra, T ## _DF) (T i, T j) { return BUILTIN (sra, T ## _DF) (i, j); }
#define SRAI(T) NOMIPS16 T FN (srai, T ## _DF) (T i) { return BUILTIN (srai, T ## _DF) (i, 1); }
#define SRAR(T) NOMIPS16 T FN (srar, T ## _DF) (T i, T j) { return BUILTIN (srar, T ## _DF) (i, j); }
#define SRARI(T) NOMIPS16 T FN (srari, T ## _DF) (T i) { return BUILTIN (srari, T ## _DF) (i, 0); }
#define SRL(T) NOMIPS16 T FN (srl, T ## _DF) (T i, T j) { return BUILTIN (srl, T ## _DF) (i, j); }
#define SRLI(T) NOMIPS16 T FN (srli, T ## _DF) (T i) { return BUILTIN (srli, T ## _DF) (i, 1); }
#define SRLR(T) NOMIPS16 T FN (srlr, T ## _DF) (T i, T j) { return BUILTIN (srlr, T ## _DF) (i, j); }
#define SRLRI(T) NOMIPS16 T FN (srlri, T ## _DF) (T i) { return BUILTIN (srlri, T ## _DF) (i, 0); }

/* MSA Floating-Point Arithmetic builtins.  */
#define FADD(T) NOMIPS16 T FN (fadd, T ## _DF) (T i, T j) { return BUILTIN (fadd, T ## _DF) (i, j); }
#define FDIV(T) NOMIPS16 T FN (fdiv, T ## _DF) (T i, T j) { return BUILTIN (fdiv, T ## _DF) (i, j); }
#define FEXP2(T) NOMIPS16 T FN (fexp2, T ## _DF) (T i, T ## _FEXP2 j) { return BUILTIN (fexp2, T ## _DF) (i, j); }
#define FLOG2(T) NOMIPS16 T FN (flog2, T ## _DF) (T i) { return BUILTIN (flog2, T ## _DF) (i); }
#define FMADD(T) NOMIPS16 T FN (fmadd, T ## _DF) (T i, T j, T k) { return BUILTIN (fmadd, T ## _DF) (i, j, k); }
#define FMSUB(T) NOMIPS16 T FN (fmsub, T ## _DF) (T i, T j, T k) { return BUILTIN (fmsub, T ## _DF) (i, j, k); }
#define FMAX(T) NOMIPS16 T FN (fmax, T ## _DF) (T i, T j) { return BUILTIN (fmax, T ## _DF) (i, j); }
#define FMIN(T) NOMIPS16 T FN (fmin, T ## _DF) (T i, T j) { return BUILTIN (fmin, T ## _DF) (i, j); }
#define FMAX_A(T) NOMIPS16 T FN (fmax_a, T ## _DF) (T i, T j) { return BUILTIN (fmax_a, T ## _DF) (i, j); }
#define FMIN_A(T) NOMIPS16 T FN (fmin_a, T ## _DF) (T i, T j) { return BUILTIN (fmin_a, T ## _DF) (i, j); }
#define FMUL(T) NOMIPS16 T FN (fmul, T ## _DF) (T i, T j) { return BUILTIN (fmul, T ## _DF) (i, j); }
#define FRCP(T) NOMIPS16 T FN (frcp, T ## _DF) (T i) { return BUILTIN (frcp, T ## _DF) (i); }
#define FRINT(T) NOMIPS16 T FN (frint, T ## _DF) (T i) { return BUILTIN (frint, T ## _DF) (i); }
#define FRSQRT(T) NOMIPS16 T FN (frsqrt, T ## _DF) (T i) { return BUILTIN (frsqrt, T ## _DF) (i); }
#define FSQRT(T) NOMIPS16 T FN (fsqrt, T ## _DF) (T i) { return BUILTIN (fsqrt, T ## _DF) (i); }
#define FSUB(T) NOMIPS16 T FN (fsub, T ## _DF) (T i, T j) { return BUILTIN (fsub, T ## _DF) (i, j); }

/* MSA Floating-Point Compare builtins.  */
#define FCLASS(T) NOMIPS16 T ## _FRES FN (fclass, T ## _DF) (T i) { return BUILTIN (fclass, T ## _DF) (i); }
#define FCAF(T) NOMIPS16 T ## _FRES FN (fcaf, T ## _DF) (T i, T j) { return BUILTIN (fcaf, T ## _DF) (i, j); }
#define FCUN(T) NOMIPS16 T ## _FRES FN (fcun, T ## _DF) (T i, T j) { return BUILTIN (fcun, T ## _DF) (i, j); }
#define FCOR(T) NOMIPS16 T ## _FRES FN (fcor, T ## _DF) (T i, T j) { return BUILTIN (fcor, T ## _DF) (i, j); }
#define FCEQ(T) NOMIPS16 T ## _FRES FN (fceq, T ## _DF) (T i, T j) { return BUILTIN (fceq, T ## _DF) (i, j); }
#define FCUNE(T) NOMIPS16 T ## _FRES FN (fcune, T ## _DF) (T i, T j) { return BUILTIN (fcune, T ## _DF) (i, j); }
#define FCUEQ(T) NOMIPS16 T ## _FRES FN (fcueq, T ## _DF) (T i, T j) { return BUILTIN (fcueq, T ## _DF) (i, j); }
#define FCNE(T) NOMIPS16 T ## _FRES FN (fcne, T ## _DF) (T i, T j) { return BUILTIN (fcne, T ## _DF) (i, j); }
#define FCLT(T) NOMIPS16 T ## _FRES FN (fclt, T ## _DF) (T i, T j) { return BUILTIN (fclt, T ## _DF) (i, j); }
#define FCULT(T) NOMIPS16 T ## _FRES FN (fcult, T ## _DF) (T i, T j) { return BUILTIN (fcult, T ## _DF) (i, j); }
#define FCLE(T) NOMIPS16 T ## _FRES FN (fcle, T ## _DF) (T i, T j) { return BUILTIN (fcle, T ## _DF) (i, j); }
#define FCULE(T) NOMIPS16 T ## _FRES FN (fcule, T ## _DF) (T i, T j) { return BUILTIN (fcule, T ## _DF) (i, j); }
#define FSAF(T) NOMIPS16 T ## _FRES FN (fsaf, T ## _DF) (T i, T j) { return BUILTIN (fsaf, T ## _DF) (i, j); }
#define FSUN(T) NOMIPS16 T ## _FRES FN (fsun, T ## _DF) (T i, T j) { return BUILTIN (fsun, T ## _DF) (i, j); }
#define FSOR(T) NOMIPS16 T ## _FRES FN (fsor, T ## _DF) (T i, T j) { return BUILTIN (fsor, T ## _DF) (i, j); }
#define FSEQ(T) NOMIPS16 T ## _FRES FN (fseq, T ## _DF) (T i, T j) { return BUILTIN (fseq, T ## _DF) (i, j); }
#define FSUNE(T) NOMIPS16 T ## _FRES FN (fsune, T ## _DF) (T i, T j) { return BUILTIN (fsune, T ## _DF) (i, j); }
#define FSUEQ(T) NOMIPS16 T ## _FRES FN (fsueq, T ## _DF) (T i, T j) { return BUILTIN (fsueq, T ## _DF) (i, j); }
#define FSNE(T) NOMIPS16 T ## _FRES FN (fsne, T ## _DF) (T i, T j) { return BUILTIN (fsne, T ## _DF) (i, j); }
#define FSLT(T) NOMIPS16 T ## _FRES FN (fslt, T ## _DF) (T i, T j) { return BUILTIN (fslt, T ## _DF) (i, j); }
#define FSULT(T) NOMIPS16 T ## _FRES FN (fsult, T ## _DF) (T i, T j) { return BUILTIN (fsult, T ## _DF) (i, j); }
#define FSLE(T) NOMIPS16 T ## _FRES FN (fsle, T ## _DF) (T i, T j) { return BUILTIN (fsle, T ## _DF) (i, j); }
#define FSULE(T) NOMIPS16 T ## _FRES FN (fsule, T ## _DF) (T i, T j) { return BUILTIN (fsule, T ## _DF) (i, j); }

/* MSA Floating-Point Conversion builtins.  */
#define FEXUPL(T) NOMIPS16 T FN (fexupl, T ## _DF) (T ## _FCNV i) { return BUILTIN (fexupl, T ## _DF) (i); }
#define FEXUPR(T) NOMIPS16 T FN (fexupr, T ## _DF) (T ## _FCNV i) { return BUILTIN (fexupr, T ## _DF) (i); }
#define FEXDO(T) NOMIPS16 T ## _FCNV FN (fexdo, T ## _HDF) (T i, T j) { return BUILTIN (fexdo, T ## _HDF) (i, j); }
#define FFINT_S(T) NOMIPS16 T FN (ffint_s, T ## _DF) (T ## _FSINT i) { return BUILTIN (ffint_s, T ## _DF) (i); }
#define FFINT_U(T) NOMIPS16 T FN (ffint_u, T ## _DF) (T ## _FUINT i) { return BUILTIN (ffint_u, T ## _DF) (i); }
#define FFQL(T) NOMIPS16 T FN (ffql, T ## _DF) (T ## _FFP i) { return BUILTIN (ffql, T ## _DF) (i); }
#define FFQR(T) NOMIPS16 T FN (ffqr, T ## _DF) (T ## _FFP i) { return BUILTIN (ffqr, T ## _DF) (i); }
#define FTINT_S(T) NOMIPS16 T ## _FSINT FN (ftint_s, T ## _DF) (T i) { return BUILTIN (ftint_s, T ## _DF) (i); }
#define FTINT_U(T) NOMIPS16 T ## _FUINT FN (ftint_u, T ## _DF) (T i) { return BUILTIN (ftint_u, T ## _DF) (i); }
#define FTRUNC_S(T) NOMIPS16 T ## _FSINT FN (ftrunc_s, T ## _DF) (T i) { return BUILTIN (ftrunc_s, T ## _DF) (i); }
#define FTRUNC_U(T) NOMIPS16 T ## _FUINT FN (ftrunc_u, T ## _DF) (T i) { return BUILTIN (ftrunc_u, T ## _DF) (i); }
#define FTQ(T) NOMIPS16 T ## _FFP FN (ftq, T ## _HDF) (T i, T j) { return BUILTIN (ftq, T ## _HDF) (i, j); }

/* MSA Fixed-Point Multiplication builtins.  */
#define MADD_Q(T) NOMIPS16 T ## _FFP FN (madd_q, T ## _HDF) (T ## _FFP i, T ## _FFP j, T ## _FFP k) { return BUILTIN (madd_q, T ## _HDF) (i, j, k); }
#define MADDR_Q(T) NOMIPS16 T ## _FFP FN (maddr_q, T ## _HDF) (T ## _FFP i, T ## _FFP j, T ## _FFP k) { return BUILTIN (maddr_q, T ## _HDF) (i, j, k); }
#define MSUB_Q(T) NOMIPS16 T ## _FFP FN (msub_q, T ## _HDF) (T ## _FFP i, T ## _FFP j, T ## _FFP k) { return BUILTIN (msub_q, T ## _HDF) (i, j, k); }
#define MSUBR_Q(T) NOMIPS16 T ## _FFP FN (msubr_q, T ## _HDF) (T ## _FFP i, T ## _FFP j, T ## _FFP k) { return BUILTIN (msubr_q, T ## _HDF) (i, j, k); }
#define MUL_Q(T) NOMIPS16 T ## _FFP FN (mul_q, T ## _HDF) (T ## _FFP i, T ## _FFP j) { return BUILTIN (mul_q, T ## _HDF) (i, j); }
#define MULR_Q(T) NOMIPS16 T ## _FFP FN (mulr_q, T ## _HDF) (T ## _FFP i, T ## _FFP j) { return BUILTIN (mulr_q, T ## _HDF) (i, j); }

/* MSA Compare builtins.  */
#define CEQ(T) NOMIPS16 T FN (ceq, T ## _DF) (T i, T j) { return BUILTIN (ceq, T ## _DF) (i, j); }
#define CEQI(T) NOMIPS16 T FN (ceqi, T ## _DF) (T i) { return BUILTIN (ceqi, T ## _DF) (i, 0); }
#define CLE_S(T) NOMIPS16 T FN (cle_s, T ## _DF) (T i, T j) { return BUILTIN (cle_s, T ## _DF) (i, j); }
#define CLEI_S(T) NOMIPS16 T FN (clei_s, T ## _DF) (T i) { return BUILTIN (clei_s, T ## _DF) (i, 0); }
#define CLE_U(T) NOMIPS16 T ## _CMP FN (cle_u, T ## _DF) (T i, T j) { return BUILTIN (cle_u, T ## _DF) (i, j); }
#define CLEI_U(T) NOMIPS16 T ## _CMP FN (clei_u, T ## _DF) (T i) { return BUILTIN (clei_u, T ## _DF) (i, 10); }
#define CLT_S(T) NOMIPS16 T FN (clt_s, T ## _DF) (T i, T j) { return BUILTIN (clt_s, T ## _DF) (i, j); }
#define CLTI_S(T) NOMIPS16 T FN (clti_s, T ## _DF) (T i) { return BUILTIN (clti_s, T ## _DF) (i, 0); }
#define CLT_U(T) NOMIPS16 T ## _CMP FN (clt_u, T ## _DF) (T i, T j) { return BUILTIN (clt_u, T ## _DF) (i, j); }
#define CLTI_U(T) NOMIPS16 T ## _CMP FN (clti_u, T ## _DF) (T i) { return BUILTIN (clti_u, T ## _DF) (i, 0); }

/* MSA Branch builtins.  */
#define BNZV(T) NOMIPS16 int FN (bnz, v) (T i) { return BUILTIN (bnz, v) (i); }
#define BZV(T) NOMIPS16 int FN (bz, v) (T i) { return BUILTIN (bz, v) (i); }
#define BNZ(T) NOMIPS16 int FN (bnz, T ## _DF) (T i) { return BUILTIN (bnz, T ## _DF) (i); }
#define BZ(T) NOMIPS16 int FN (bz, T ## _DF) (T i) { return BUILTIN (bz, T ## _DF) (i); }

/* MSA Load/Store and Move builtins.  */
#define CFCMSA() int msa_cfcmsa () { return __builtin_msa_cfcmsa(0x1f); }
#define CTCMSA() void msa_ctcmsa (int i) { return __builtin_msa_ctcmsa(0x1f, i); }
#define LD(T) T FN (ld, T ## _DF) (char *i) { return BUILTIN (ld, T ## _DF) (i, 0); }
#define LDI(T) T FN (ldi, T ## _DF) () { return BUILTIN (ldi, T ## _DF) (123); }
#define MOVE(T) NOMIPS16 T FN (move, v) (T i) { return BUILTIN (move, v) (i); }
#define SPLAT(T) T FN (splat, T ## _DF) (T i, int j) { return BUILTIN (splat, T ## _DF) (i, j); }
#define SPLATI(T) T FN (splati, T ## _DF) (T i) { return BUILTIN (splati, T ## _DF) (i, 1); }
#define FILL(T) T FN (fill, T ## _DF) (int i) { return BUILTIN (fill, T ## _DF) (i); }
#define INSERT(T) T FN (insert, T ## _DF) (T i, int j) { return BUILTIN (insert, T ## _DF) (i, 1, j); }
#define INSVE(T) T FN (insve, T ## _DF) (T i, T j) { return BUILTIN (insve, T ## _DF) (i, 1, j); }
#define COPY_S(T) int FN (copy_s, T ## _DF) (T i) { return BUILTIN (copy_s, T ## _DF) (i, 1); }
#define COPY_S_D(T) long long FN (copy_s, T ## _DF) (T i) { return BUILTIN (copy_s, T ## _DF) (i, 1); }
#define COPY_U(T) unsigned int FN (copy_u, T ## _DF) (T i) { return BUILTIN (copy_u, T ## _DF) (i, 1); }
#define COPY_U_D(T) unsigned long long FN (copy_u, T ## _DF) (T i) { return BUILTIN (copy_u, T ## _DF) (i, 1); }
#define ST(T) void FN (st, T ## _DF) (T i, char *j) { BUILTIN (st, T ## _DF) (i, j, -64); }

/* MSA Element Permute builtins.  */
#define ILVEV(T) NOMIPS16 T FN (ilvev, T ## _DF) (T i, T j) { return BUILTIN (ilvev, T ## _DF) (i, j); }
#define ILVOD(T) NOMIPS16 T FN (ilvod, T ## _DF) (T i, T j) { return BUILTIN (ilvod, T ## _DF) (i, j); }
#define ILVL(T) NOMIPS16 T FN (ilvl, T ## _DF) (T i, T j) { return BUILTIN (ilvl, T ## _DF) (i, j); }
#define ILVR(T) NOMIPS16 T FN (ilvr, T ## _DF) (T i, T j) { return BUILTIN (ilvr, T ## _DF) (i, j); }
#define PCKEV(T) NOMIPS16 T FN (pckev, T ## _DF) (T i, T j) { return BUILTIN (pckev, T ## _DF) (i, j); }
#define PCKOD(T) NOMIPS16 T FN (pckod, T ## _DF) (T i, T j) { return BUILTIN (pckod, T ## _DF) (i, j); }
#define SHF(T) NOMIPS16 T FN (shf, T ## _DF) (T i) { return BUILTIN (shf, T ## _DF) (i, 127); }
#define SLD(T) NOMIPS16 T FN (sld, T ## _DF) (T i, T j, int k) { return BUILTIN (sld, T ## _DF) (i, j, k); }
#define SLDI(T) NOMIPS16 T FN (sldi, T ## _DF) (T i, T j) { return BUILTIN (sldi, T ## _DF) (i, j, 1); }
#define VSHF(T) NOMIPS16 T FN (vshf, T ## _DF) (T i, T j, T k) { return BUILTIN (vshf, T ## _DF) (i, j, k); }

/* GCC builtins that generate MSA instructions.  */
#define SHUFFLE1_S(T) T FN (gcc_1_s_vshf, T ## _DF) (T i, T mask) { return __builtin_shuffle (i, mask); }
#define SHUFFLE1_U(T) T FN (gcc_1_u_vshf, T ## _DF) (T i, T mask) { return __builtin_shuffle (i, mask); }
#define SHUFFLE2_S(T) T FN (gcc_2_s_vshf, T ## _DF) (T i, T j, T mask) { return __builtin_shuffle (i, j, mask); }
#define SHUFFLE2_U(T) T FN (gcc_2_u_vshf, T ## _DF) (T i, T j, T mask) { return __builtin_shuffle (i, j, mask); }
#define REAL_SHUFFLE1(T, MASK_T) T FN (gcc_3_vshf, T ## _DF) (T i, MASK_T mask) { return __builtin_shuffle (i, mask); }
#define REAL_SHUFFLE2(T, MASK_T) T FN (gcc_4_vshf, T ## _DF) (T i, T j, MASK_T mask) { return __builtin_shuffle (i, j, mask); }

#define ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES(FUNC) \
  FUNC (v16i8) \
  FUNC (v8i16) \
  FUNC (v4i32) \
  FUNC (v2i64)

#define ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2(FUNC) \
  FUNC (v16i8) \
  FUNC (v8i16) \
  FUNC (v4i32)

#define ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES(FUNC) \
  FUNC (v16u8) \
  FUNC (v8u16) \
  FUNC (v4u32) \
  FUNC (v2u64)

#define ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES_2(FUNC) \
  FUNC (v16u8) \
  FUNC (v8u16) \
  FUNC (v4u32)

#define ITERATE_FOR_ALL_REAL_VECTOR_TYPES(FUNC) \
  FUNC (v4f32) \
  FUNC (v2f64) \

/* MSA Arithmetic builtins.  */
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ADDV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ADDVI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ADD_A)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ADDS_A)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ADDS_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (ADDS_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (HADD_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES_2 (HADD_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ASUB_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (ASUB_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (AVE_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (AVE_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (AVER_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (AVER_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (DOTP_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES_2 (DOTP_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (DPADD_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES_2 (DPADD_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (DPSUB_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES_2 (DPSUB_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (DIV_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (DIV_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MADDV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MAX_A)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MIN_A)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MAX_S)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MAXI_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (MAX_U)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (MAXI_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MIN_S)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MINI_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (MIN_U)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (MINI_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MSUBV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MULV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (MOD_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (MOD_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SAT_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (SAT_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SUBS_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (SUBS_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (HSUB_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES_2 (HSUB_U)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (SUBSUU_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (SUBSUS_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SUBV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SUBVI)

/* MSA Bitwise builtins.  */
AND (v16u8)
ANDI (v16u8)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BCLR)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BCLRI)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BINSL)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BINSLI)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BINSR)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BINSRI)
BMNZ (v16u8)
BMNZI (v16u8)
BMZ (v16u8)
BMZI (v16u8)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BNEG)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BNEGI)
BSEL (v16u8)
BSELI (v16u8)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BSET)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BSETI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (NLOC)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (NLZC)
NOR (v16u8)
NORI (v16u8)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (PCNT)
OR (v16u8)
ORI (v16u8)
XOR (v16u8)
XORI (v16u8)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SLL)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SLLI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRA)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRAI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRAR)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRARI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRL)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRLI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRLR)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SRLRI)

/* MSA Floating-Point Arithmetic builtins.  */
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FADD)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FDIV)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FEXP2)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FLOG2)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMADD)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMSUB)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMAX)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMIN)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMAX_A)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMIN_A)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FMUL)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FRCP)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FRINT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FRSQRT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSQRT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSUB)

/* MSA Floating-Point Compare builtins.  */
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCLASS)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCAF)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCUN)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCOR)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCEQ)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCUNE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCUEQ)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCNE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCLT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCULT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCLE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FCULE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSAF)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSUN)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSOR)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSEQ)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSUNE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSUEQ)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSNE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSLT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSULT)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSLE)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FSULE)

/* MSA Floating-Point Conversion builtins.  */
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FEXUPL)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FEXUPR)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FEXDO)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FFINT_S)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FFINT_U)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FFQL)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FFQR)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FTINT_S)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FTINT_U)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FTRUNC_S)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FTRUNC_U)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (FTQ)

/* MSA Fixed-Point Multiplication builtins.  */
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (MADD_Q)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (MADDR_Q)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (MSUB_Q)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (MSUBR_Q)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (MUL_Q)
ITERATE_FOR_ALL_REAL_VECTOR_TYPES (MULR_Q)

/* MSA Compare builtins.  */
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (CEQ)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (CEQI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (CLE_S)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (CLEI_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (CLE_U)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (CLEI_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (CLT_S)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (CLTI_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (CLT_U)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (CLTI_U)

/* MSA Branch builtins.  */
BNZV (v16u8)
BZV (v16u8)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BNZ)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (BZ)

/* MSA Load/Store and Move builtins.  */
CFCMSA ()
CTCMSA ()
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (LD)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (LDI)
MOVE (v16i8)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SPLAT)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SPLATI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (FILL)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (INSERT)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (INSVE)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (COPY_S)
COPY_S_D (v2i64)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (COPY_U)
COPY_U_D (v2i64)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ST)

/* MSA Element Permute builtins.  */
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ILVEV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ILVOD)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ILVL)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (ILVR)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (PCKEV)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (PCKOD)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES_2 (SHF)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SLD)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SLDI)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (VSHF)

/* GCC builtins.  */
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SHUFFLE1_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (SHUFFLE1_U)
ITERATE_FOR_ALL_SIGNED_INT_VECTOR_TYPES (SHUFFLE2_S)
ITERATE_FOR_ALL_UNSIGNED_INT_VECTOR_TYPES (SHUFFLE2_U)
REAL_SHUFFLE1 (v2f64, v2i64)
REAL_SHUFFLE2 (v2f64, v2i64)
REAL_SHUFFLE1 (v4f32, v4i32)
REAL_SHUFFLE2 (v4f32, v4i32)
