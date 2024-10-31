/* Test builtins for LOONGARCH LASX ASE instructions */
/* { dg-do compile } */
/* { dg-options "-mlasx" } */
/* { dg-final { scan-assembler-times "lasx_xvsll_b:.*xvsll\\.b.*lasx_xvsll_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsll_h:.*xvsll\\.h.*lasx_xvsll_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsll_w:.*xvsll\\.w.*lasx_xvsll_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsll_d:.*xvsll\\.d.*lasx_xvsll_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslli_b:.*xvslli\\.b.*lasx_xvslli_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslli_h:.*xvslli\\.h.*lasx_xvslli_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslli_w:.*xvslli\\.w.*lasx_xvslli_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslli_d:.*xvslli\\.d.*lasx_xvslli_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsra_b:.*xvsra\\.b.*lasx_xvsra_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsra_h:.*xvsra\\.h.*lasx_xvsra_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsra_w:.*xvsra\\.w.*lasx_xvsra_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsra_d:.*xvsra\\.d.*lasx_xvsra_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrai_b:.*xvsrai\\.b.*lasx_xvsrai_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrai_h:.*xvsrai\\.h.*lasx_xvsrai_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrai_w:.*xvsrai\\.w.*lasx_xvsrai_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrai_d:.*xvsrai\\.d.*lasx_xvsrai_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrar_b:.*xvsrar\\.b.*lasx_xvsrar_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrar_h:.*xvsrar\\.h.*lasx_xvsrar_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrar_w:.*xvsrar\\.w.*lasx_xvsrar_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrar_d:.*xvsrar\\.d.*lasx_xvsrar_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrari_b:.*xvsrari\\.b.*lasx_xvsrari_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrari_h:.*xvsrari\\.h.*lasx_xvsrari_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrari_w:.*xvsrari\\.w.*lasx_xvsrari_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrari_d:.*xvsrari\\.d.*lasx_xvsrari_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrl_b:.*xvsrl\\.b.*lasx_xvsrl_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrl_h:.*xvsrl\\.h.*lasx_xvsrl_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrl_w:.*xvsrl\\.w.*lasx_xvsrl_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrl_d:.*xvsrl\\.d.*lasx_xvsrl_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrli_b:.*xvsrli\\.b.*lasx_xvsrli_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrli_h:.*xvsrli\\.h.*lasx_xvsrli_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrli_w:.*xvsrli\\.w.*lasx_xvsrli_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrli_d:.*xvsrli\\.d.*lasx_xvsrli_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlr_b:.*xvsrlr\\.b.*lasx_xvsrlr_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlr_h:.*xvsrlr\\.h.*lasx_xvsrlr_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlr_w:.*xvsrlr\\.w.*lasx_xvsrlr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlr_d:.*xvsrlr\\.d.*lasx_xvsrlr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlri_b:.*xvsrlri\\.b.*lasx_xvsrlri_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlri_h:.*xvsrlri\\.h.*lasx_xvsrlri_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlri_w:.*xvsrlri\\.w.*lasx_xvsrlri_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlri_d:.*xvsrlri\\.d.*lasx_xvsrlri_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclr_b:.*xvbitclr\\.b.*lasx_xvbitclr_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclr_h:.*xvbitclr\\.h.*lasx_xvbitclr_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclr_w:.*xvbitclr\\.w.*lasx_xvbitclr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclr_d:.*xvbitclr\\.d.*lasx_xvbitclr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclri_b:.*xvbitclri\\.b.*lasx_xvbitclri_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclri_h:.*xvbitclri\\.h.*lasx_xvbitclri_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclri_w:.*xvbitclri\\.w.*lasx_xvbitclri_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitclri_d:.*xvbitclri\\.d.*lasx_xvbitclri_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitset_b:.*xvbitset\\.b.*lasx_xvbitset_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitset_h:.*xvbitset\\.h.*lasx_xvbitset_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitset_w:.*xvbitset\\.w.*lasx_xvbitset_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitset_d:.*xvbitset\\.d.*lasx_xvbitset_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitseti_b:.*xvbitseti\\.b.*lasx_xvbitseti_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitseti_h:.*xvbitseti\\.h.*lasx_xvbitseti_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitseti_w:.*xvbitseti\\.w.*lasx_xvbitseti_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitseti_d:.*xvbitseti\\.d.*lasx_xvbitseti_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrev_b:.*xvbitrev\\.b.*lasx_xvbitrev_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrev_h:.*xvbitrev\\.h.*lasx_xvbitrev_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrev_w:.*xvbitrev\\.w.*lasx_xvbitrev_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrev_d:.*xvbitrev\\.d.*lasx_xvbitrev_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrevi_b:.*xvbitrevi\\.b.*lasx_xvbitrevi_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrevi_h:.*xvbitrevi\\.h.*lasx_xvbitrevi_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrevi_w:.*xvbitrevi\\.w.*lasx_xvbitrevi_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitrevi_d:.*xvbitrevi\\.d.*lasx_xvbitrevi_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadd_b:.*xvadd\\.b.*lasx_xvadd_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadd_h:.*xvadd\\.h.*lasx_xvadd_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadd_w:.*xvadd\\.w.*lasx_xvadd_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadd_d:.*xvadd\\.d.*lasx_xvadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddi_bu:.*xvaddi\\.bu.*lasx_xvaddi_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddi_hu:.*xvaddi\\.hu.*lasx_xvaddi_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddi_wu:.*xvaddi\\.wu.*lasx_xvaddi_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddi_du:.*xvaddi\\.du.*lasx_xvaddi_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsub_b:.*xvsub\\.b.*lasx_xvsub_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsub_h:.*xvsub\\.h.*lasx_xvsub_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsub_w:.*xvsub\\.w.*lasx_xvsub_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsub_d:.*xvsub\\.d.*lasx_xvsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubi_bu:.*xvsubi\\.bu.*lasx_xvsubi_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubi_hu:.*xvsubi\\.hu.*lasx_xvsubi_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubi_wu:.*xvsubi\\.wu.*lasx_xvsubi_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubi_du:.*xvsubi\\.du.*lasx_xvsubi_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_b:.*xvmax\\.b.*lasx_xvmax_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_h:.*xvmax\\.h.*lasx_xvmax_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_w:.*xvmax\\.w.*lasx_xvmax_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_d:.*xvmax\\.d.*lasx_xvmax_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_b:.*xvmaxi\\.b.*lasx_xvmaxi_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_h:.*xvmaxi\\.h.*lasx_xvmaxi_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_w:.*xvmaxi\\.w.*lasx_xvmaxi_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_d:.*xvmaxi\\.d.*lasx_xvmaxi_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_bu:.*xvmax\\.bu.*lasx_xvmax_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_hu:.*xvmax\\.hu.*lasx_xvmax_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_wu:.*xvmax\\.wu.*lasx_xvmax_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmax_du:.*xvmax\\.du.*lasx_xvmax_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_bu:.*xvmaxi\\.bu.*lasx_xvmaxi_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_hu:.*xvmaxi\\.hu.*lasx_xvmaxi_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_wu:.*xvmaxi\\.wu.*lasx_xvmaxi_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaxi_du:.*xvmaxi\\.du.*lasx_xvmaxi_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_b:.*xvmin\\.b.*lasx_xvmin_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_h:.*xvmin\\.h.*lasx_xvmin_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_w:.*xvmin\\.w.*lasx_xvmin_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_d:.*xvmin\\.d.*lasx_xvmin_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_b:.*xvmini\\.b.*lasx_xvmini_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_h:.*xvmini\\.h.*lasx_xvmini_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_w:.*xvmini\\.w.*lasx_xvmini_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_d:.*xvmini\\.d.*lasx_xvmini_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_bu:.*xvmin\\.bu.*lasx_xvmin_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_hu:.*xvmin\\.hu.*lasx_xvmin_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_wu:.*xvmin\\.wu.*lasx_xvmin_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmin_du:.*xvmin\\.du.*lasx_xvmin_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_bu:.*xvmini\\.bu.*lasx_xvmini_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_hu:.*xvmini\\.hu.*lasx_xvmini_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_wu:.*xvmini\\.wu.*lasx_xvmini_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmini_du:.*xvmini\\.du.*lasx_xvmini_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseq_b:.*xvseq\\.b.*lasx_xvseq_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseq_h:.*xvseq\\.h.*lasx_xvseq_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseq_w:.*xvseq\\.w.*lasx_xvseq_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseq_d:.*xvseq\\.d.*lasx_xvseq_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseqi_b:.*xvseqi\\.b.*lasx_xvseqi_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseqi_h:.*xvseqi\\.h.*lasx_xvseqi_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseqi_w:.*xvseqi\\.w.*lasx_xvseqi_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvseqi_d:.*xvseqi\\.d.*lasx_xvseqi_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_b:.*xvslt\\.b.*lasx_xvslt_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_h:.*xvslt\\.h.*lasx_xvslt_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_w:.*xvslt\\.w.*lasx_xvslt_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_d:.*xvslt\\.d.*lasx_xvslt_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_b:.*xvslti\\.b.*lasx_xvslti_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_h:.*xvslti\\.h.*lasx_xvslti_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_w:.*xvslti\\.w.*lasx_xvslti_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_d:.*xvslti\\.d.*lasx_xvslti_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_bu:.*xvslt\\.bu.*lasx_xvslt_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_hu:.*xvslt\\.hu.*lasx_xvslt_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_wu:.*xvslt\\.wu.*lasx_xvslt_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslt_du:.*xvslt\\.du.*lasx_xvslt_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_bu:.*xvslti\\.bu.*lasx_xvslti_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_hu:.*xvslti\\.hu.*lasx_xvslti_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_wu:.*xvslti\\.wu.*lasx_xvslti_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslti_du:.*xvslti\\.du.*lasx_xvslti_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_b:.*xvsle\\.b.*lasx_xvsle_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_h:.*xvsle\\.h.*lasx_xvsle_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_w:.*xvsle\\.w.*lasx_xvsle_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_d:.*xvsle\\.d.*lasx_xvsle_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_b:.*xvslei\\.b.*lasx_xvslei_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_h:.*xvslei\\.h.*lasx_xvslei_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_w:.*xvslei\\.w.*lasx_xvslei_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_d:.*xvslei\\.d.*lasx_xvslei_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_bu:.*xvsle\\.bu.*lasx_xvsle_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_hu:.*xvsle\\.hu.*lasx_xvsle_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_wu:.*xvsle\\.wu.*lasx_xvsle_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsle_du:.*xvsle\\.du.*lasx_xvsle_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_bu:.*xvslei\\.bu.*lasx_xvslei_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_hu:.*xvslei\\.hu.*lasx_xvslei_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_wu:.*xvslei\\.wu.*lasx_xvslei_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvslei_du:.*xvslei\\.du.*lasx_xvslei_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_b:.*xvsat\\.b.*lasx_xvsat_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_h:.*xvsat\\.h.*lasx_xvsat_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_w:.*xvsat\\.w.*lasx_xvsat_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_d:.*xvsat\\.d.*lasx_xvsat_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_bu:.*xvsat\\.bu.*lasx_xvsat_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_hu:.*xvsat\\.hu.*lasx_xvsat_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_wu:.*xvsat\\.wu.*lasx_xvsat_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsat_du:.*xvsat\\.du.*lasx_xvsat_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadda_b:.*xvadda\\.b.*lasx_xvadda_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadda_h:.*xvadda\\.h.*lasx_xvadda_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadda_w:.*xvadda\\.w.*lasx_xvadda_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadda_d:.*xvadda\\.d.*lasx_xvadda_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_b:.*xvsadd\\.b.*lasx_xvsadd_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_h:.*xvsadd\\.h.*lasx_xvsadd_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_w:.*xvsadd\\.w.*lasx_xvsadd_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_d:.*xvsadd\\.d.*lasx_xvsadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_bu:.*xvsadd\\.bu.*lasx_xvsadd_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_hu:.*xvsadd\\.hu.*lasx_xvsadd_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_wu:.*xvsadd\\.wu.*lasx_xvsadd_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsadd_du:.*xvsadd\\.du.*lasx_xvsadd_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_b:.*xvavg\\.b.*lasx_xvavg_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_h:.*xvavg\\.h.*lasx_xvavg_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_w:.*xvavg\\.w.*lasx_xvavg_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_d:.*xvavg\\.d.*lasx_xvavg_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_bu:.*xvavg\\.bu.*lasx_xvavg_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_hu:.*xvavg\\.hu.*lasx_xvavg_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_wu:.*xvavg\\.wu.*lasx_xvavg_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavg_du:.*xvavg\\.du.*lasx_xvavg_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_b:.*xvavgr\\.b.*lasx_xvavgr_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_h:.*xvavgr\\.h.*lasx_xvavgr_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_w:.*xvavgr\\.w.*lasx_xvavgr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_d:.*xvavgr\\.d.*lasx_xvavgr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_bu:.*xvavgr\\.bu.*lasx_xvavgr_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_hu:.*xvavgr\\.hu.*lasx_xvavgr_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_wu:.*xvavgr\\.wu.*lasx_xvavgr_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvavgr_du:.*xvavgr\\.du.*lasx_xvavgr_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_b:.*xvssub\\.b.*lasx_xvssub_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_h:.*xvssub\\.h.*lasx_xvssub_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_w:.*xvssub\\.w.*lasx_xvssub_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_d:.*xvssub\\.d.*lasx_xvssub_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_bu:.*xvssub\\.bu.*lasx_xvssub_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_hu:.*xvssub\\.hu.*lasx_xvssub_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_wu:.*xvssub\\.wu.*lasx_xvssub_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssub_du:.*xvssub\\.du.*lasx_xvssub_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_b:.*xvabsd\\.b.*lasx_xvabsd_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_h:.*xvabsd\\.h.*lasx_xvabsd_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_w:.*xvabsd\\.w.*lasx_xvabsd_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_d:.*xvabsd\\.d.*lasx_xvabsd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_bu:.*xvabsd\\.bu.*lasx_xvabsd_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_hu:.*xvabsd\\.hu.*lasx_xvabsd_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_wu:.*xvabsd\\.wu.*lasx_xvabsd_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvabsd_du:.*xvabsd\\.du.*lasx_xvabsd_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmul_b:.*xvmul\\.b.*lasx_xvmul_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmul_h:.*xvmul\\.h.*lasx_xvmul_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmul_w:.*xvmul\\.w.*lasx_xvmul_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmul_d:.*xvmul\\.d.*lasx_xvmul_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmadd_b:.*xvmadd\\.b.*lasx_xvmadd_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmadd_h:.*xvmadd\\.h.*lasx_xvmadd_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmadd_w:.*xvmadd\\.w.*lasx_xvmadd_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmadd_d:.*xvmadd\\.d.*lasx_xvmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmsub_b:.*xvmsub\\.b.*lasx_xvmsub_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmsub_h:.*xvmsub\\.h.*lasx_xvmsub_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmsub_w:.*xvmsub\\.w.*lasx_xvmsub_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmsub_d:.*xvmsub\\.d.*lasx_xvmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_b:.*xvdiv\\.b.*lasx_xvdiv_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_h:.*xvdiv\\.h.*lasx_xvdiv_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_w:.*xvdiv\\.w.*lasx_xvdiv_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_d:.*xvdiv\\.d.*lasx_xvdiv_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_bu:.*xvdiv\\.bu.*lasx_xvdiv_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_hu:.*xvdiv\\.hu.*lasx_xvdiv_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_wu:.*xvdiv\\.wu.*lasx_xvdiv_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvdiv_du:.*xvdiv\\.du.*lasx_xvdiv_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_h_b:.*xvhaddw\\.h\\.b.*lasx_xvhaddw_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_w_h:.*xvhaddw\\.w\\.h.*lasx_xvhaddw_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_d_w:.*xvhaddw\\.d\\.w.*lasx_xvhaddw_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_hu_bu:.*xvhaddw\\.hu\\.bu.*lasx_xvhaddw_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_wu_hu:.*xvhaddw\\.wu\\.hu.*lasx_xvhaddw_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_du_wu:.*xvhaddw\\.du\\.wu.*lasx_xvhaddw_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_h_b:.*xvhsubw\\.h\\.b.*lasx_xvhsubw_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_w_h:.*xvhsubw\\.w\\.h.*lasx_xvhsubw_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_d_w:.*xvhsubw\\.d\\.w.*lasx_xvhsubw_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_hu_bu:.*xvhsubw\\.hu\\.bu.*lasx_xvhsubw_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_wu_hu:.*xvhsubw\\.wu\\.hu.*lasx_xvhsubw_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_du_wu:.*xvhsubw\\.du\\.wu.*lasx_xvhsubw_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_b:.*xvmod\\.b.*lasx_xvmod_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_h:.*xvmod\\.h.*lasx_xvmod_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_w:.*xvmod\\.w.*lasx_xvmod_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_d:.*xvmod\\.d.*lasx_xvmod_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_bu:.*xvmod\\.bu.*lasx_xvmod_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_hu:.*xvmod\\.hu.*lasx_xvmod_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_wu:.*xvmod\\.wu.*lasx_xvmod_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmod_du:.*xvmod\\.du.*lasx_xvmod_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepl128vei_b:.*xvrepl128vei\\.b.*lasx_xvrepl128vei_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepl128vei_h:.*xvrepl128vei\\.h.*lasx_xvrepl128vei_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepl128vei_w:.*xvrepl128vei\\.w.*lasx_xvrepl128vei_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepl128vei_d:.*xvrepl128vei\\.d.*lasx_xvrepl128vei_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickev_b:.*xvpickev\\.b.*lasx_xvpickev_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickev_h:.*xvpickev\\.h.*lasx_xvpickev_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickev_w:.*xvpickev\\.w.*lasx_xvpickev_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickev_d:.*xvilvl\\.d.*lasx_xvpickev_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickod_b:.*xvpickod\\.b.*lasx_xvpickod_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickod_h:.*xvpickod\\.h.*lasx_xvpickod_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickod_w:.*xvpickod\\.w.*lasx_xvpickod_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickod_d:.*xvilvh\\.d.*lasx_xvpickod_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvh_b:.*xvilvh\\.b.*lasx_xvilvh_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvh_h:.*xvilvh\\.h.*lasx_xvilvh_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvh_w:.*xvilvh\\.w.*lasx_xvilvh_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvh_d:.*xvilvh\\.d.*lasx_xvilvh_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvl_b:.*xvilvl\\.b.*lasx_xvilvl_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvl_h:.*xvilvl\\.h.*lasx_xvilvl_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvl_w:.*xvilvl\\.w.*lasx_xvilvl_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvilvl_d:.*xvilvl\\.d.*lasx_xvilvl_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackev_b:.*xvpackev\\.b.*lasx_xvpackev_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackev_h:.*xvpackev\\.h.*lasx_xvpackev_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackev_w:.*xvpackev\\.w.*lasx_xvpackev_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackev_d:.*xvilvl\\.d.*lasx_xvpackev_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackod_b:.*xvpackod\\.b.*lasx_xvpackod_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackod_h:.*xvpackod\\.h.*lasx_xvpackod_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackod_w:.*xvpackod\\.w.*lasx_xvpackod_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpackod_d:.*xvilvh\\.d.*lasx_xvpackod_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf_b:.*xvshuf\\.b.*lasx_xvshuf_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf_h:.*xvshuf\\.h.*lasx_xvshuf_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf_w:.*xvshuf\\.w.*lasx_xvshuf_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf_d:.*xvshuf\\.d.*lasx_xvshuf_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvand_v:.*xvand\\.v.*lasx_xvand_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvandi_b:.*xvandi\\.b.*lasx_xvandi_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvor_v:.*xvor\\.v.*lasx_xvor_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvori_b:.*xvbitseti\\.b.*lasx_xvori_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvnor_v:.*xvnor\\.v.*lasx_xvnor_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvnori_b:.*xvnori\\.b.*lasx_xvnori_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvxor_v:.*xvxor\\.v.*lasx_xvxor_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvxori_b:.*xvbitrevi\\.b.*lasx_xvxori_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitsel_v:.*xvbitsel\\.v.*lasx_xvbitsel_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbitseli_b:.*xvbitseli\\.b.*lasx_xvbitseli_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf4i_b:.*xvshuf4i\\.b.*lasx_xvshuf4i_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf4i_h:.*xvshuf4i\\.h.*lasx_xvshuf4i_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf4i_w:.*xvshuf4i\\.w.*lasx_xvshuf4i_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplgr2vr_b:.*xvreplgr2vr\\.b.*lasx_xvreplgr2vr_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplgr2vr_h:.*xvreplgr2vr\\.h.*lasx_xvreplgr2vr_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplgr2vr_w:.*xvreplgr2vr\\.w.*lasx_xvreplgr2vr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplgr2vr_d:.*xvreplgr2vr\\.d.*lasx_xvreplgr2vr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpcnt_b:.*xvpcnt\\.b.*lasx_xvpcnt_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpcnt_h:.*xvpcnt\\.h.*lasx_xvpcnt_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpcnt_w:.*xvpcnt\\.w.*lasx_xvpcnt_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpcnt_d:.*xvpcnt\\.d.*lasx_xvpcnt_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclo_b:.*xvclo\\.b.*lasx_xvclo_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclo_h:.*xvclo\\.h.*lasx_xvclo_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclo_w:.*xvclo\\.w.*lasx_xvclo_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclo_d:.*xvclo\\.d.*lasx_xvclo_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclz_b:.*xvclz\\.b.*lasx_xvclz_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclz_h:.*xvclz\\.h.*lasx_xvclz_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclz_w:.*xvclz\\.w.*lasx_xvclz_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvclz_d:.*xvclz\\.d.*lasx_xvclz_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfadd_s:.*xvfadd\\.s.*lasx_xvfadd_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfadd_d:.*xvfadd\\.d.*lasx_xvfadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfsub_s:.*xvfsub\\.s.*lasx_xvfsub_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfsub_d:.*xvfsub\\.d.*lasx_xvfsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmul_s:.*xvfmul\\.s.*lasx_xvfmul_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmul_d:.*xvfmul\\.d.*lasx_xvfmul_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfdiv_s:.*xvfdiv\\.s.*lasx_xvfdiv_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfdiv_d:.*xvfdiv\\.d.*lasx_xvfdiv_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcvt_h_s:.*xvfcvt\\.h\\.s.*lasx_xvfcvt_h_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcvt_s_d:.*xvfcvt\\.s\\.d.*lasx_xvfcvt_s_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmin_s:.*xvfmin\\.s.*lasx_xvfmin_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmin_d:.*xvfmin\\.d.*lasx_xvfmin_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmina_s:.*xvfmina\\.s.*lasx_xvfmina_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmina_d:.*xvfmina\\.d.*lasx_xvfmina_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmax_s:.*xvfmax\\.s.*lasx_xvfmax_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmax_d:.*xvfmax\\.d.*lasx_xvfmax_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmaxa_s:.*xvfmaxa\\.s.*lasx_xvfmaxa_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmaxa_d:.*xvfmaxa\\.d.*lasx_xvfmaxa_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfclass_s:.*xvfclass\\.s.*lasx_xvfclass_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfclass_d:.*xvfclass\\.d.*lasx_xvfclass_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfsqrt_s:.*xvfsqrt\\.s.*lasx_xvfsqrt_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfsqrt_d:.*xvfsqrt\\.d.*lasx_xvfsqrt_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrecip_s:.*xvfrecip\\.s.*lasx_xvfrecip_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrecip_d:.*xvfrecip\\.d.*lasx_xvfrecip_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrint_s:.*xvfrint\\.s.*lasx_xvfrint_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrint_d:.*xvfrint\\.d.*lasx_xvfrint_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrsqrt_s:.*xvfrsqrt\\.s.*lasx_xvfrsqrt_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrsqrt_d:.*xvfrsqrt\\.d.*lasx_xvfrsqrt_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvflogb_s:.*xvflogb\\.s.*lasx_xvflogb_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvflogb_d:.*xvflogb\\.d.*lasx_xvflogb_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcvth_s_h:.*xvfcvth\\.s\\.h.*lasx_xvfcvth_s_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcvth_d_s:.*xvfcvth\\.d\\.s.*lasx_xvfcvth_d_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcvtl_s_h:.*xvfcvtl\\.s\\.h.*lasx_xvfcvtl_s_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcvtl_d_s:.*xvfcvtl\\.d\\.s.*lasx_xvfcvtl_d_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftint_w_s:.*xvftint\\.w\\.s.*lasx_xvftint_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftint_l_d:.*xvftint\\.l\\.d.*lasx_xvftint_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftint_wu_s:.*xvftint\\.wu\\.s.*lasx_xvftint_wu_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftint_lu_d:.*xvftint\\.lu\\.d.*lasx_xvftint_lu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrz_w_s:.*xvftintrz\\.w\\.s.*lasx_xvftintrz_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrz_l_d:.*xvftintrz\\.l\\.d.*lasx_xvftintrz_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrz_wu_s:.*xvftintrz\\.wu\\.s.*lasx_xvftintrz_wu_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrz_lu_d:.*xvftintrz\\.lu\\.d.*lasx_xvftintrz_lu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffint_s_w:.*xvffint\\.s\\.w.*lasx_xvffint_s_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffint_d_l:.*xvffint\\.d\\.l.*lasx_xvffint_d_l" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffint_s_wu:.*xvffint\\.s\\.wu.*lasx_xvffint_s_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffint_d_lu:.*xvffint\\.d\\.lu.*lasx_xvffint_d_lu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve_b:.*xvreplve\\.b.*lasx_xvreplve_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve_h:.*xvreplve\\.h.*lasx_xvreplve_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve_w:.*xvreplve\\.w.*lasx_xvreplve_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve_d:.*xvreplve\\.d.*lasx_xvreplve_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpermi_w:.*xvpermi\\.w.*lasx_xvpermi_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvandn_v:.*xvandn\\.v.*lasx_xvandn_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvneg_b:.*xvneg\\.b.*lasx_xvneg_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvneg_h:.*xvneg\\.h.*lasx_xvneg_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvneg_w:.*xvneg\\.w.*lasx_xvneg_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvneg_d:.*xvneg\\.d.*lasx_xvneg_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_b:.*xvmuh\\.b.*lasx_xvmuh_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_h:.*xvmuh\\.h.*lasx_xvmuh_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_w:.*xvmuh\\.w.*lasx_xvmuh_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_d:.*xvmuh\\.d.*lasx_xvmuh_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_bu:.*xvmuh\\.bu.*lasx_xvmuh_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_hu:.*xvmuh\\.hu.*lasx_xvmuh_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_wu:.*xvmuh\\.wu.*lasx_xvmuh_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmuh_du:.*xvmuh\\.du.*lasx_xvmuh_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsllwil_h_b:.*xvsllwil\\.h\\.b.*lasx_xvsllwil_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsllwil_w_h:.*xvsllwil\\.w\\.h.*lasx_xvsllwil_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsllwil_d_w:.*xvsllwil\\.d\\.w.*lasx_xvsllwil_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsllwil_hu_bu:.*xvsllwil\\.hu\\.bu.*lasx_xvsllwil_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsllwil_wu_hu:.*xvsllwil\\.wu\\.hu.*lasx_xvsllwil_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsllwil_du_wu:.*xvsllwil\\.du\\.wu.*lasx_xvsllwil_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsran_b_h:.*xvsran\\.b\\.h.*lasx_xvsran_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsran_h_w:.*xvsran\\.h\\.w.*lasx_xvsran_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsran_w_d:.*xvsran\\.w\\.d.*lasx_xvsran_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssran_b_h:.*xvssran\\.b\\.h.*lasx_xvssran_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssran_h_w:.*xvssran\\.h\\.w.*lasx_xvssran_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssran_w_d:.*xvssran\\.w\\.d.*lasx_xvssran_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssran_bu_h:.*xvssran\\.bu\\.h.*lasx_xvssran_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssran_hu_w:.*xvssran\\.hu\\.w.*lasx_xvssran_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssran_wu_d:.*xvssran\\.wu\\.d.*lasx_xvssran_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarn_b_h:.*xvsrarn\\.b\\.h.*lasx_xvsrarn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarn_h_w:.*xvsrarn\\.h\\.w.*lasx_xvsrarn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarn_w_d:.*xvsrarn\\.w\\.d.*lasx_xvsrarn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarn_b_h:.*xvssrarn\\.b\\.h.*lasx_xvssrarn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarn_h_w:.*xvssrarn\\.h\\.w.*lasx_xvssrarn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarn_w_d:.*xvssrarn\\.w\\.d.*lasx_xvssrarn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarn_bu_h:.*xvssrarn\\.bu\\.h.*lasx_xvssrarn_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarn_hu_w:.*xvssrarn\\.hu\\.w.*lasx_xvssrarn_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarn_wu_d:.*xvssrarn\\.wu\\.d.*lasx_xvssrarn_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrln_b_h:.*xvsrln\\.b\\.h.*lasx_xvsrln_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrln_h_w:.*xvsrln\\.h\\.w.*lasx_xvsrln_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrln_w_d:.*xvsrln\\.w\\.d.*lasx_xvsrln_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrln_bu_h:.*xvssrln\\.bu\\.h.*lasx_xvssrln_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrln_hu_w:.*xvssrln\\.hu\\.w.*lasx_xvssrln_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrln_wu_d:.*xvssrln\\.wu\\.d.*lasx_xvssrln_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrn_b_h:.*xvsrlrn\\.b\\.h.*lasx_xvsrlrn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrn_h_w:.*xvsrlrn\\.h\\.w.*lasx_xvsrlrn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrn_w_d:.*xvsrlrn\\.w\\.d.*lasx_xvsrlrn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrn_bu_h:.*xvssrlrn\\.bu\\.h.*lasx_xvssrlrn_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrn_hu_w:.*xvssrlrn\\.hu\\.w.*lasx_xvssrlrn_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrn_wu_d:.*xvssrlrn\\.wu\\.d.*lasx_xvssrlrn_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrstpi_b:.*xvfrstpi\\.b.*lasx_xvfrstpi_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrstpi_h:.*xvfrstpi\\.h.*lasx_xvfrstpi_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrstp_b:.*xvfrstp\\.b.*lasx_xvfrstp_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrstp_h:.*xvfrstp\\.h.*lasx_xvfrstp_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvshuf4i_d:.*xvshuf4i\\.d.*lasx_xvshuf4i_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbsrl_v:.*xvbsrl\\.v.*lasx_xvbsrl_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvbsll_v:.*xvbsll\\.v.*lasx_xvbsll_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvextrins_b:.*xvextrins\\.b.*lasx_xvextrins_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvextrins_h:.*xvextrins\\.h.*lasx_xvextrins_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvextrins_w:.*xvextrins\\.w.*lasx_xvextrins_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvextrins_d:.*xvextrins\\.d.*lasx_xvextrins_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmskltz_b:.*xvmskltz\\.b.*lasx_xvmskltz_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmskltz_h:.*xvmskltz\\.h.*lasx_xvmskltz_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmskltz_w:.*xvmskltz\\.w.*lasx_xvmskltz_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmskltz_d:.*xvmskltz\\.d.*lasx_xvmskltz_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsigncov_b:.*xvsigncov\\.b.*lasx_xvsigncov_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsigncov_h:.*xvsigncov\\.h.*lasx_xvsigncov_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsigncov_w:.*xvsigncov\\.w.*lasx_xvsigncov_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsigncov_d:.*xvsigncov\\.d.*lasx_xvsigncov_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmadd_s:.*xvfmadd\\.s.*lasx_xvfmadd_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmadd_d:.*xvfmadd\\.d.*lasx_xvfmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmsub_s:.*xvfmsub\\.s.*lasx_xvfmsub_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfmsub_d:.*xvfmsub\\.d.*lasx_xvfmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfnmadd_s:.*xvfnmadd\\.s.*lasx_xvfnmadd_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfnmadd_d:.*xvfnmadd\\.d.*lasx_xvfnmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfnmsub_s:.*xvfnmsub\\.s.*lasx_xvfnmsub_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfnmsub_d:.*xvfnmsub\\.d.*lasx_xvfnmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrne_w_s:.*xvftintrne\\.w\\.s.*lasx_xvftintrne_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrne_l_d:.*xvftintrne\\.l\\.d.*lasx_xvftintrne_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrp_w_s:.*xvftintrp\\.w\\.s.*lasx_xvftintrp_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrp_l_d:.*xvftintrp\\.l\\.d.*lasx_xvftintrp_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrm_w_s:.*xvftintrm\\.w\\.s.*lasx_xvftintrm_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrm_l_d:.*xvftintrm\\.l\\.d.*lasx_xvftintrm_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftint_w_d:.*xvftint\\.w\\.d.*lasx_xvftint_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffint_s_l:.*xvffint\\.s\\.l.*lasx_xvffint_s_l" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrz_w_d:.*xvftintrz\\.w\\.d.*lasx_xvftintrz_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrp_w_d:.*xvftintrp\\.w\\.d.*lasx_xvftintrp_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrm_w_d:.*xvftintrm\\.w\\.d.*lasx_xvftintrm_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrne_w_d:.*xvftintrne\\.w\\.d.*lasx_xvftintrne_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftinth_l_s:.*xvftinth\\.l\\.s.*lasx_xvftinth_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintl_l_s:.*xvftintl\\.l\\.s.*lasx_xvftintl_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffinth_d_w:.*xvffinth\\.d\\.w.*lasx_xvffinth_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvffintl_d_w:.*xvffintl\\.d\\.w.*lasx_xvffintl_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrzh_l_s:.*xvftintrzh\\.l\\.s.*lasx_xvftintrzh_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrzl_l_s:.*xvftintrzl\\.l\\.s.*lasx_xvftintrzl_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrph_l_s:.*xvftintrph\\.l\\.s.*lasx_xvftintrph_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrpl_l_s:.*xvftintrpl\\.l\\.s.*lasx_xvftintrpl_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrmh_l_s:.*xvftintrmh\\.l\\.s.*lasx_xvftintrmh_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrml_l_s:.*xvftintrml\\.l\\.s.*lasx_xvftintrml_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrneh_l_s:.*xvftintrneh\\.l\\.s.*lasx_xvftintrneh_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvftintrnel_l_s:.*xvftintrnel\\.l\\.s.*lasx_xvftintrnel_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrne_s:.*xvfrintrne\\.s.*lasx_xvfrintrne_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrne_d:.*xvfrintrne\\.d.*lasx_xvfrintrne_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrz_s:.*xvfrintrz\\.s.*lasx_xvfrintrz_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrz_d:.*xvfrintrz\\.d.*lasx_xvfrintrz_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrp_s:.*xvfrintrp\\.s.*lasx_xvfrintrp_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrp_d:.*xvfrintrp\\.d.*lasx_xvfrintrp_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrm_s:.*xvfrintrm\\.s.*lasx_xvfrintrm_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrintrm_d:.*xvfrintrm\\.d.*lasx_xvfrintrm_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvld:.*xvld.*lasx_xvld" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvst:.*xvst.*lasx_xvst" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvstelm_b:.*xvstelm\\.b.*lasx_xvstelm_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvstelm_h:.*xvstelm\\.h.*lasx_xvstelm_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvstelm_w:.*xvstelm\\.w.*lasx_xvstelm_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvstelm_d:.*xvstelm\\.d.*lasx_xvstelm_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvinsve0_w:.*xvinsve0\\.w.*lasx_xvinsve0_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvinsve0_d:.*xvinsve0\\.d.*lasx_xvinsve0_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve_w:.*xvpickve\\.w.*lasx_xvpickve_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve_d:.*xvpickve\\.d.*lasx_xvpickve_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrn_b_h:.*xvssrlrn\\.b\\.h.*lasx_xvssrlrn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrn_h_w:.*xvssrlrn\\.h\\.w.*lasx_xvssrlrn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrn_w_d:.*xvssrlrn\\.w\\.d.*lasx_xvssrlrn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrln_b_h:.*xvssrln\\.b\\.h.*lasx_xvssrln_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrln_h_w:.*xvssrln\\.h\\.w.*lasx_xvssrln_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrln_w_d:.*xvssrln\\.w\\.d.*lasx_xvssrln_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvorn_v:.*xvorn\\.v.*lasx_xvorn_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvldi:.*xvldi.*lasx_xvldi" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvldx:.*xvldx.*lasx_xvldx" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvstx:.*xvstx.*lasx_xvstx" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvextl_qu_du:.*xvextl\\.qu\\.du.*lasx_xvextl_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvinsgr2vr_w:.*xvinsgr2vr\\.w.*lasx_xvinsgr2vr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvinsgr2vr_d:.*xvinsgr2vr\\.d.*lasx_xvinsgr2vr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve0_b:.*xvreplve0\\.b.*lasx_xvreplve0_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve0_h:.*xvreplve0\\.h.*lasx_xvreplve0_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve0_w:.*xvreplve0\\.w.*lasx_xvreplve0_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve0_d:.*xvreplve0\\.d.*lasx_xvreplve0_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvreplve0_q:.*xvreplve0\\.q.*lasx_xvreplve0_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_h_b:.*vext2xv\\.h\\.b.*lasx_vext2xv_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_w_h:.*vext2xv\\.w\\.h.*lasx_vext2xv_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_d_w:.*vext2xv\\.d\\.w.*lasx_vext2xv_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_w_b:.*vext2xv\\.w\\.b.*lasx_vext2xv_w_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_d_h:.*vext2xv\\.d\\.h.*lasx_vext2xv_d_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_d_b:.*vext2xv\\.d\\.b.*lasx_vext2xv_d_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_hu_bu:.*vext2xv\\.hu\\.bu.*lasx_vext2xv_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_wu_hu:.*vext2xv\\.wu\\.hu.*lasx_vext2xv_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_du_wu:.*vext2xv\\.du\\.wu.*lasx_vext2xv_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_wu_bu:.*vext2xv\\.wu\\.bu.*lasx_vext2xv_wu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_du_hu:.*vext2xv\\.du\\.hu.*lasx_vext2xv_du_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_vext2xv_du_bu:.*vext2xv\\.du\\.bu.*lasx_vext2xv_du_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpermi_q:.*xvpermi\\.q.*lasx_xvpermi_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpermi_d:.*xvpermi\\.d.*lasx_xvpermi_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvperm_w:.*xvperm\\.w.*lasx_xvperm_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvldrepl_b:.*xvldrepl\\.b.*lasx_xvldrepl_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvldrepl_h:.*xvldrepl\\.h.*lasx_xvldrepl_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvldrepl_w:.*xvldrepl\\.w.*lasx_xvldrepl_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvldrepl_d:.*xvldrepl\\.d.*lasx_xvldrepl_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve2gr_w:.*xvpickve2gr\\.w.*lasx_xvpickve2gr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve2gr_wu:.*xvpickve2gr\\.wu.*lasx_xvpickve2gr_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve2gr_d:.*xvpickve2gr\\.d.*lasx_xvpickve2gr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve2gr_du:.*xvpickve2gr\\.du.*lasx_xvpickve2gr_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_q_d:.*xvaddwev\\.q\\.d.*lasx_xvaddwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_d_w:.*xvaddwev\\.d\\.w.*lasx_xvaddwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_w_h:.*xvaddwev\\.w\\.h.*lasx_xvaddwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_h_b:.*xvaddwev\\.h\\.b.*lasx_xvaddwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_q_du:.*xvaddwev\\.q\\.du.*lasx_xvaddwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_d_wu:.*xvaddwev\\.d\\.wu.*lasx_xvaddwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_w_hu:.*xvaddwev\\.w\\.hu.*lasx_xvaddwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_h_bu:.*xvaddwev\\.h\\.bu.*lasx_xvaddwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_q_d:.*xvsubwev\\.q\\.d.*lasx_xvsubwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_d_w:.*xvsubwev\\.d\\.w.*lasx_xvsubwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_w_h:.*xvsubwev\\.w\\.h.*lasx_xvsubwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_h_b:.*xvsubwev\\.h\\.b.*lasx_xvsubwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_q_du:.*xvsubwev\\.q\\.du.*lasx_xvsubwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_d_wu:.*xvsubwev\\.d\\.wu.*lasx_xvsubwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_w_hu:.*xvsubwev\\.w\\.hu.*lasx_xvsubwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwev_h_bu:.*xvsubwev\\.h\\.bu.*lasx_xvsubwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_q_d:.*xvmulwev\\.q\\.d.*lasx_xvmulwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_d_w:.*xvmulwev\\.d\\.w.*lasx_xvmulwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_w_h:.*xvmulwev\\.w\\.h.*lasx_xvmulwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_h_b:.*xvmulwev\\.h\\.b.*lasx_xvmulwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_q_du:.*xvmulwev\\.q\\.du.*lasx_xvmulwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_d_wu:.*xvmulwev\\.d\\.wu.*lasx_xvmulwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_w_hu:.*xvmulwev\\.w\\.hu.*lasx_xvmulwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_h_bu:.*xvmulwev\\.h\\.bu.*lasx_xvmulwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_q_d:.*xvaddwod\\.q\\.d.*lasx_xvaddwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_d_w:.*xvaddwod\\.d\\.w.*lasx_xvaddwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_w_h:.*xvaddwod\\.w\\.h.*lasx_xvaddwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_h_b:.*xvaddwod\\.h\\.b.*lasx_xvaddwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_q_du:.*xvaddwod\\.q\\.du.*lasx_xvaddwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_d_wu:.*xvaddwod\\.d\\.wu.*lasx_xvaddwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_w_hu:.*xvaddwod\\.w\\.hu.*lasx_xvaddwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_h_bu:.*xvaddwod\\.h\\.bu.*lasx_xvaddwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_q_d:.*xvsubwod\\.q\\.d.*lasx_xvsubwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_d_w:.*xvsubwod\\.d\\.w.*lasx_xvsubwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_w_h:.*xvsubwod\\.w\\.h.*lasx_xvsubwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_h_b:.*xvsubwod\\.h\\.b.*lasx_xvsubwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_q_du:.*xvsubwod\\.q\\.du.*lasx_xvsubwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_d_wu:.*xvsubwod\\.d\\.wu.*lasx_xvsubwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_w_hu:.*xvsubwod\\.w\\.hu.*lasx_xvsubwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsubwod_h_bu:.*xvsubwod\\.h\\.bu.*lasx_xvsubwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_q_d:.*xvmulwod\\.q\\.d.*lasx_xvmulwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_d_w:.*xvmulwod\\.d\\.w.*lasx_xvmulwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_w_h:.*xvmulwod\\.w\\.h.*lasx_xvmulwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_h_b:.*xvmulwod\\.h\\.b.*lasx_xvmulwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_q_du:.*xvmulwod\\.q\\.du.*lasx_xvmulwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_d_wu:.*xvmulwod\\.d\\.wu.*lasx_xvmulwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_w_hu:.*xvmulwod\\.w\\.hu.*lasx_xvmulwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_h_bu:.*xvmulwod\\.h\\.bu.*lasx_xvmulwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_d_wu_w:.*xvaddwev\\.d\\.wu\\.w.*lasx_xvaddwev_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_w_hu_h:.*xvaddwev\\.w\\.hu\\.h.*lasx_xvaddwev_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_h_bu_b:.*xvaddwev\\.h\\.bu\\.b.*lasx_xvaddwev_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_d_wu_w:.*xvmulwev\\.d\\.wu\\.w.*lasx_xvmulwev_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_w_hu_h:.*xvmulwev\\.w\\.hu\\.h.*lasx_xvmulwev_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_h_bu_b:.*xvmulwev\\.h\\.bu\\.b.*lasx_xvmulwev_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_d_wu_w:.*xvaddwod\\.d\\.wu\\.w.*lasx_xvaddwod_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_w_hu_h:.*xvaddwod\\.w\\.hu\\.h.*lasx_xvaddwod_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_h_bu_b:.*xvaddwod\\.h\\.bu\\.b.*lasx_xvaddwod_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_d_wu_w:.*xvmulwod\\.d\\.wu\\.w.*lasx_xvmulwod_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_w_hu_h:.*xvmulwod\\.w\\.hu\\.h.*lasx_xvmulwod_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_h_bu_b:.*xvmulwod\\.h\\.bu\\.b.*lasx_xvmulwod_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_q_d:.*xvhaddw\\.q\\.d.*lasx_xvhaddw_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhaddw_qu_du:.*xvhaddw\\.qu\\.du.*lasx_xvhaddw_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_q_d:.*xvhsubw\\.q\\.d.*lasx_xvhsubw_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvhsubw_qu_du:.*xvhsubw\\.qu\\.du.*lasx_xvhsubw_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_q_d:.*xvmaddwev\\.q\\.d.*lasx_xvmaddwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_d_w:.*xvmaddwev\\.d\\.w.*lasx_xvmaddwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_w_h:.*xvmaddwev\\.w\\.h.*lasx_xvmaddwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_h_b:.*xvmaddwev\\.h\\.b.*lasx_xvmaddwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_q_du:.*xvmaddwev\\.q\\.du.*lasx_xvmaddwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_d_wu:.*xvmaddwev\\.d\\.wu.*lasx_xvmaddwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_w_hu:.*xvmaddwev\\.w\\.hu.*lasx_xvmaddwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_h_bu:.*xvmaddwev\\.h\\.bu.*lasx_xvmaddwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_q_d:.*xvmaddwod\\.q\\.d.*lasx_xvmaddwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_d_w:.*xvmaddwod\\.d\\.w.*lasx_xvmaddwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_w_h:.*xvmaddwod\\.w\\.h.*lasx_xvmaddwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_h_b:.*xvmaddwod\\.h\\.b.*lasx_xvmaddwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_q_du:.*xvmaddwod\\.q\\.du.*lasx_xvmaddwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_d_wu:.*xvmaddwod\\.d\\.wu.*lasx_xvmaddwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_w_hu:.*xvmaddwod\\.w\\.hu.*lasx_xvmaddwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_h_bu:.*xvmaddwod\\.h\\.bu.*lasx_xvmaddwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_q_du_d:.*xvmaddwev\\.q\\.du\\.d.*lasx_xvmaddwev_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_d_wu_w:.*xvmaddwev\\.d\\.wu\\.w.*lasx_xvmaddwev_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_w_hu_h:.*xvmaddwev\\.w\\.hu\\.h.*lasx_xvmaddwev_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwev_h_bu_b:.*xvmaddwev\\.h\\.bu\\.b.*lasx_xvmaddwev_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_q_du_d:.*xvmaddwod\\.q\\.du\\.d.*lasx_xvmaddwod_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_d_wu_w:.*xvmaddwod\\.d\\.wu\\.w.*lasx_xvmaddwod_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_w_hu_h:.*xvmaddwod\\.w\\.hu\\.h.*lasx_xvmaddwod_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmaddwod_h_bu_b:.*xvmaddwod\\.h\\.bu\\.b.*lasx_xvmaddwod_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotr_b:.*xvrotr\\.b.*lasx_xvrotr_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotr_h:.*xvrotr\\.h.*lasx_xvrotr_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotr_w:.*xvrotr\\.w.*lasx_xvrotr_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotr_d:.*xvrotr\\.d.*lasx_xvrotr_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvadd_q:.*xvadd\\.q.*lasx_xvadd_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsub_q:.*xvsub\\.q.*lasx_xvsub_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwev_q_du_d:.*xvaddwev\\.q\\.du\\.d.*lasx_xvaddwev_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvaddwod_q_du_d:.*xvaddwod\\.q\\.du\\.d.*lasx_xvaddwod_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwev_q_du_d:.*xvmulwev\\.q\\.du\\.d.*lasx_xvmulwev_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmulwod_q_du_d:.*xvmulwod\\.q\\.du\\.d.*lasx_xvmulwod_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmskgez_b:.*xvmskgez\\.b.*lasx_xvmskgez_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvmsknz_b:.*xvmsknz\\.b.*lasx_xvmsknz_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_h_b:.*xvexth\\.h\\.b.*lasx_xvexth_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_w_h:.*xvexth\\.w\\.h.*lasx_xvexth_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_d_w:.*xvexth\\.d\\.w.*lasx_xvexth_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_q_d:.*xvexth\\.q\\.d.*lasx_xvexth_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_hu_bu:.*xvexth\\.hu\\.bu.*lasx_xvexth_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_wu_hu:.*xvexth\\.wu\\.hu.*lasx_xvexth_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_du_wu:.*xvexth\\.du\\.wu.*lasx_xvexth_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvexth_qu_du:.*xvexth\\.qu\\.du.*lasx_xvexth_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotri_b:.*xvrotri\\.b.*lasx_xvrotri_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotri_h:.*xvrotri\\.h.*lasx_xvrotri_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotri_w:.*xvrotri\\.w.*lasx_xvrotri_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrotri_d:.*xvrotri\\.d.*lasx_xvrotri_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvextl_q_d:.*xvextl\\.q\\.d.*lasx_xvextl_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlni_b_h:.*xvsrlni\\.b\\.h.*lasx_xvsrlni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlni_h_w:.*xvsrlni\\.h\\.w.*lasx_xvsrlni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlni_w_d:.*xvsrlni\\.w\\.d.*lasx_xvsrlni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlni_d_q:.*xvsrlni\\.d\\.q.*lasx_xvsrlni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrni_b_h:.*xvsrlrni\\.b\\.h.*lasx_xvsrlrni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrni_h_w:.*xvsrlrni\\.h\\.w.*lasx_xvsrlrni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrni_w_d:.*xvsrlrni\\.w\\.d.*lasx_xvsrlrni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrlrni_d_q:.*xvsrlrni\\.d\\.q.*lasx_xvsrlrni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_b_h:.*xvssrlni\\.b\\.h.*lasx_xvssrlni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_h_w:.*xvssrlni\\.h\\.w.*lasx_xvssrlni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_w_d:.*xvssrlni\\.w\\.d.*lasx_xvssrlni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_d_q:.*xvssrlni\\.d\\.q.*lasx_xvssrlni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_bu_h:.*xvssrlni\\.bu\\.h.*lasx_xvssrlni_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_hu_w:.*xvssrlni\\.hu\\.w.*lasx_xvssrlni_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_wu_d:.*xvssrlni\\.wu\\.d.*lasx_xvssrlni_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlni_du_q:.*xvssrlni\\.du\\.q.*lasx_xvssrlni_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_b_h:.*xvssrlrni\\.b\\.h.*lasx_xvssrlrni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_h_w:.*xvssrlrni\\.h\\.w.*lasx_xvssrlrni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_w_d:.*xvssrlrni\\.w\\.d.*lasx_xvssrlrni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_d_q:.*xvssrlrni\\.d\\.q.*lasx_xvssrlrni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_bu_h:.*xvssrlrni\\.bu\\.h.*lasx_xvssrlrni_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_hu_w:.*xvssrlrni\\.hu\\.w.*lasx_xvssrlrni_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_wu_d:.*xvssrlrni\\.wu\\.d.*lasx_xvssrlrni_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrlrni_du_q:.*xvssrlrni\\.du\\.q.*lasx_xvssrlrni_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrani_b_h:.*xvsrani\\.b\\.h.*lasx_xvsrani_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrani_h_w:.*xvsrani\\.h\\.w.*lasx_xvsrani_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrani_w_d:.*xvsrani\\.w\\.d.*lasx_xvsrani_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrani_d_q:.*xvsrani\\.d\\.q.*lasx_xvsrani_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarni_b_h:.*xvsrarni\\.b\\.h.*lasx_xvsrarni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarni_h_w:.*xvsrarni\\.h\\.w.*lasx_xvsrarni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarni_w_d:.*xvsrarni\\.w\\.d.*lasx_xvsrarni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvsrarni_d_q:.*xvsrarni\\.d\\.q.*lasx_xvsrarni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_b_h:.*xvssrani\\.b\\.h.*lasx_xvssrani_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_h_w:.*xvssrani\\.h\\.w.*lasx_xvssrani_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_w_d:.*xvssrani\\.w\\.d.*lasx_xvssrani_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_d_q:.*xvssrani\\.d\\.q.*lasx_xvssrani_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_bu_h:.*xvssrani\\.bu\\.h.*lasx_xvssrani_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_hu_w:.*xvssrani\\.hu\\.w.*lasx_xvssrani_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_wu_d:.*xvssrani\\.wu\\.d.*lasx_xvssrani_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrani_du_q:.*xvssrani\\.du\\.q.*lasx_xvssrani_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_b_h:.*xvssrarni\\.b\\.h.*lasx_xvssrarni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_h_w:.*xvssrarni\\.h\\.w.*lasx_xvssrarni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_w_d:.*xvssrarni\\.w\\.d.*lasx_xvssrarni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_d_q:.*xvssrarni\\.d\\.q.*lasx_xvssrarni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_bu_h:.*xvssrarni\\.bu\\.h.*lasx_xvssrarni_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_hu_w:.*xvssrarni\\.hu\\.w.*lasx_xvssrarni_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_wu_d:.*xvssrarni\\.wu\\.d.*lasx_xvssrarni_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvssrarni_du_q:.*xvssrarni\\.du\\.q.*lasx_xvssrarni_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbnz_b:.*xvsetanyeqz\\.b.*lasx_xbnz_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbnz_d:.*xvsetanyeqz\\.d.*lasx_xbnz_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbnz_h:.*xvsetanyeqz\\.h.*lasx_xbnz_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbnz_v:.*xvseteqz\\.v.*lasx_xbnz_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbnz_w:.*xvsetanyeqz\\.w.*lasx_xbnz_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbz_b:.*xvsetallnez\\.b.*lasx_xbz_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbz_d:.*xvsetallnez\\.d.*lasx_xbz_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbz_h:.*xvsetallnez\\.h.*lasx_xbz_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbz_v:.*xvsetnez\\.v.*lasx_xbz_v" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xbz_w:.*xvsetallnez\\.w.*lasx_xbz_w" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_caf_d:.*xvfcmp\\.caf\\.d.*lasx_xvfcmp_caf_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_caf_s:.*xvfcmp\\.caf\\.s.*lasx_xvfcmp_caf_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_ceq_d:.*xvfcmp\\.ceq\\.d.*lasx_xvfcmp_ceq_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_ceq_s:.*xvfcmp\\.ceq\\.s.*lasx_xvfcmp_ceq_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cle_d:.*xvfcmp\\.cle\\.d.*lasx_xvfcmp_cle_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cle_s:.*xvfcmp\\.cle\\.s.*lasx_xvfcmp_cle_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_clt_d:.*xvfcmp\\.clt\\.d.*lasx_xvfcmp_clt_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_clt_s:.*xvfcmp\\.clt\\.s.*lasx_xvfcmp_clt_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cne_d:.*xvfcmp\\.cne\\.d.*lasx_xvfcmp_cne_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cne_s:.*xvfcmp\\.cne\\.s.*lasx_xvfcmp_cne_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cor_d:.*xvfcmp\\.cor\\.d.*lasx_xvfcmp_cor_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cor_s:.*xvfcmp\\.cor\\.s.*lasx_xvfcmp_cor_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cueq_d:.*xvfcmp\\.cueq\\.d.*lasx_xvfcmp_cueq_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cueq_s:.*xvfcmp\\.cueq\\.s.*lasx_xvfcmp_cueq_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cule_d:.*xvfcmp\\.cule\\.d.*lasx_xvfcmp_cule_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cule_s:.*xvfcmp\\.cule\\.s.*lasx_xvfcmp_cule_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cult_d:.*xvfcmp\\.cult\\.d.*lasx_xvfcmp_cult_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cult_s:.*xvfcmp\\.cult\\.s.*lasx_xvfcmp_cult_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cun_d:.*xvfcmp\\.cun\\.d.*lasx_xvfcmp_cun_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cune_d:.*xvfcmp\\.cune\\.d.*lasx_xvfcmp_cune_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cune_s:.*xvfcmp\\.cune\\.s.*lasx_xvfcmp_cune_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_cun_s:.*xvfcmp\\.cun\\.s.*lasx_xvfcmp_cun_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_saf_d:.*xvfcmp\\.saf\\.d.*lasx_xvfcmp_saf_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_saf_s:.*xvfcmp\\.saf\\.s.*lasx_xvfcmp_saf_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_seq_d:.*xvfcmp\\.seq\\.d.*lasx_xvfcmp_seq_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_seq_s:.*xvfcmp\\.seq\\.s.*lasx_xvfcmp_seq_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sle_d:.*xvfcmp\\.sle\\.d.*lasx_xvfcmp_sle_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sle_s:.*xvfcmp\\.sle\\.s.*lasx_xvfcmp_sle_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_slt_d:.*xvfcmp\\.slt\\.d.*lasx_xvfcmp_slt_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_slt_s:.*xvfcmp\\.slt\\.s.*lasx_xvfcmp_slt_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sne_d:.*xvfcmp\\.sne\\.d.*lasx_xvfcmp_sne_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sne_s:.*xvfcmp\\.sne\\.s.*lasx_xvfcmp_sne_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sor_d:.*xvfcmp\\.sor\\.d.*lasx_xvfcmp_sor_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sor_s:.*xvfcmp\\.sor\\.s.*lasx_xvfcmp_sor_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sueq_d:.*xvfcmp\\.sueq\\.d.*lasx_xvfcmp_sueq_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sueq_s:.*xvfcmp\\.sueq\\.s.*lasx_xvfcmp_sueq_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sule_d:.*xvfcmp\\.sule\\.d.*lasx_xvfcmp_sule_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sule_s:.*xvfcmp\\.sule\\.s.*lasx_xvfcmp_sule_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sult_d:.*xvfcmp\\.sult\\.d.*lasx_xvfcmp_sult_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sult_s:.*xvfcmp\\.sult\\.s.*lasx_xvfcmp_sult_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sun_d:.*xvfcmp\\.sun\\.d.*lasx_xvfcmp_sun_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sune_d:.*xvfcmp\\.sune\\.d.*lasx_xvfcmp_sune_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sune_s:.*xvfcmp\\.sune\\.s.*lasx_xvfcmp_sune_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfcmp_sun_s:.*xvfcmp\\.sun\\.s.*lasx_xvfcmp_sun_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve_d_f:.*xvpickve\\.d.*lasx_xvpickve_d_f" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvpickve_w_f:.*xvpickve\\.w.*lasx_xvpickve_w_f" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepli_b:.*xvrepli\\.b.*lasx_xvrepli_b" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepli_d:.*xvrepli\\.d.*lasx_xvrepli_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepli_h:.*xvrepli\\.h.*lasx_xvrepli_h" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvrepli_w:.*xvrepli\\.w.*lasx_xvrepli_w" 1 } } */

typedef signed char v32i8 __attribute__ ((vector_size (32), aligned (32)));
typedef signed char v32i8_b __attribute__ ((vector_size (32), aligned (1)));
typedef unsigned char v32u8 __attribute__ ((vector_size (32), aligned (32)));
typedef unsigned char v32u8_b __attribute__ ((vector_size (32), aligned (1)));
typedef short v16i16 __attribute__ ((vector_size (32), aligned (32)));
typedef short v16i16_h __attribute__ ((vector_size (32), aligned (2)));
typedef unsigned short v16u16 __attribute__ ((vector_size (32), aligned (32)));
typedef unsigned short v16u16_h
    __attribute__ ((vector_size (32), aligned (2)));
typedef int v8i32 __attribute__ ((vector_size (32), aligned (32)));
typedef int v8i32_w __attribute__ ((vector_size (32), aligned (4)));
typedef unsigned int v8u32 __attribute__ ((vector_size (32), aligned (32)));
typedef unsigned int v8u32_w __attribute__ ((vector_size (32), aligned (4)));
typedef long long v4i64 __attribute__ ((vector_size (32), aligned (32)));
typedef long long v4i64_d __attribute__ ((vector_size (32), aligned (8)));
typedef unsigned long long v4u64
    __attribute__ ((vector_size (32), aligned (32)));
typedef unsigned long long v4u64_d
    __attribute__ ((vector_size (32), aligned (8)));
typedef float v8f32 __attribute__ ((vector_size (32), aligned (32)));
typedef float v8f32_w __attribute__ ((vector_size (32), aligned (4)));
typedef double v4f64 __attribute__ ((vector_size (32), aligned (32)));
typedef double v4f64_d __attribute__ ((vector_size (32), aligned (8)));

typedef double v4f64 __attribute__ ((vector_size (32), aligned (32)));
typedef double v4f64_d __attribute__ ((vector_size (32), aligned (8)));

typedef float __m256 __attribute__ ((__vector_size__ (32), __may_alias__));
typedef long long __m256i
    __attribute__ ((__vector_size__ (32), __may_alias__));
typedef double __m256d __attribute__ ((__vector_size__ (32), __may_alias__));

/* Unaligned version of the same types.  */
typedef float __m256_u
    __attribute__ ((__vector_size__ (32), __may_alias__, __aligned__ (1)));
typedef long long __m256i_u
    __attribute__ ((__vector_size__ (32), __may_alias__, __aligned__ (1)));
typedef double __m256d_u
    __attribute__ ((__vector_size__ (32), __may_alias__, __aligned__ (1)));

v32i8
__lasx_xvsll_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsll_b (_1, _2);
}
v16i16
__lasx_xvsll_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsll_h (_1, _2);
}
v8i32
__lasx_xvsll_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsll_w (_1, _2);
}
v4i64
__lasx_xvsll_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsll_d (_1, _2);
}
v32i8
__lasx_xvslli_b (v32i8 _1)
{
  return __builtin_lasx_xvslli_b (_1, 1);
}
v16i16
__lasx_xvslli_h (v16i16 _1)
{
  return __builtin_lasx_xvslli_h (_1, 1);
}
v8i32
__lasx_xvslli_w (v8i32 _1)
{
  return __builtin_lasx_xvslli_w (_1, 1);
}
v4i64
__lasx_xvslli_d (v4i64 _1)
{
  return __builtin_lasx_xvslli_d (_1, 1);
}
v32i8
__lasx_xvsra_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsra_b (_1, _2);
}
v16i16
__lasx_xvsra_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsra_h (_1, _2);
}
v8i32
__lasx_xvsra_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsra_w (_1, _2);
}
v4i64
__lasx_xvsra_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsra_d (_1, _2);
}
v32i8
__lasx_xvsrai_b (v32i8 _1)
{
  return __builtin_lasx_xvsrai_b (_1, 1);
}
v16i16
__lasx_xvsrai_h (v16i16 _1)
{
  return __builtin_lasx_xvsrai_h (_1, 1);
}
v8i32
__lasx_xvsrai_w (v8i32 _1)
{
  return __builtin_lasx_xvsrai_w (_1, 1);
}
v4i64
__lasx_xvsrai_d (v4i64 _1)
{
  return __builtin_lasx_xvsrai_d (_1, 1);
}
v32i8
__lasx_xvsrar_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrar_b (_1, _2);
}
v16i16
__lasx_xvsrar_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrar_h (_1, _2);
}
v8i32
__lasx_xvsrar_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrar_w (_1, _2);
}
v4i64
__lasx_xvsrar_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrar_d (_1, _2);
}
v32i8
__lasx_xvsrari_b (v32i8 _1)
{
  return __builtin_lasx_xvsrari_b (_1, 1);
}
v16i16
__lasx_xvsrari_h (v16i16 _1)
{
  return __builtin_lasx_xvsrari_h (_1, 1);
}
v8i32
__lasx_xvsrari_w (v8i32 _1)
{
  return __builtin_lasx_xvsrari_w (_1, 1);
}
v4i64
__lasx_xvsrari_d (v4i64 _1)
{
  return __builtin_lasx_xvsrari_d (_1, 1);
}
v32i8
__lasx_xvsrl_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrl_b (_1, _2);
}
v16i16
__lasx_xvsrl_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrl_h (_1, _2);
}
v8i32
__lasx_xvsrl_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrl_w (_1, _2);
}
v4i64
__lasx_xvsrl_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrl_d (_1, _2);
}
v32i8
__lasx_xvsrli_b (v32i8 _1)
{
  return __builtin_lasx_xvsrli_b (_1, 1);
}
v16i16
__lasx_xvsrli_h (v16i16 _1)
{
  return __builtin_lasx_xvsrli_h (_1, 1);
}
v8i32
__lasx_xvsrli_w (v8i32 _1)
{
  return __builtin_lasx_xvsrli_w (_1, 1);
}
v4i64
__lasx_xvsrli_d (v4i64 _1)
{
  return __builtin_lasx_xvsrli_d (_1, 1);
}
v32i8
__lasx_xvsrlr_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrlr_b (_1, _2);
}
v16i16
__lasx_xvsrlr_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrlr_h (_1, _2);
}
v8i32
__lasx_xvsrlr_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrlr_w (_1, _2);
}
v4i64
__lasx_xvsrlr_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrlr_d (_1, _2);
}
v32i8
__lasx_xvsrlri_b (v32i8 _1)
{
  return __builtin_lasx_xvsrlri_b (_1, 1);
}
v16i16
__lasx_xvsrlri_h (v16i16 _1)
{
  return __builtin_lasx_xvsrlri_h (_1, 1);
}
v8i32
__lasx_xvsrlri_w (v8i32 _1)
{
  return __builtin_lasx_xvsrlri_w (_1, 1);
}
v4i64
__lasx_xvsrlri_d (v4i64 _1)
{
  return __builtin_lasx_xvsrlri_d (_1, 1);
}
v32u8
__lasx_xvbitclr_b (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvbitclr_b (_1, _2);
}
v16u16
__lasx_xvbitclr_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvbitclr_h (_1, _2);
}
v8u32
__lasx_xvbitclr_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvbitclr_w (_1, _2);
}
v4u64
__lasx_xvbitclr_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvbitclr_d (_1, _2);
}
v32u8
__lasx_xvbitclri_b (v32u8 _1)
{
  return __builtin_lasx_xvbitclri_b (_1, 1);
}
v16u16
__lasx_xvbitclri_h (v16u16 _1)
{
  return __builtin_lasx_xvbitclri_h (_1, 1);
}
v8u32
__lasx_xvbitclri_w (v8u32 _1)
{
  return __builtin_lasx_xvbitclri_w (_1, 1);
}
v4u64
__lasx_xvbitclri_d (v4u64 _1)
{
  return __builtin_lasx_xvbitclri_d (_1, 1);
}
v32u8
__lasx_xvbitset_b (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvbitset_b (_1, _2);
}
v16u16
__lasx_xvbitset_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvbitset_h (_1, _2);
}
v8u32
__lasx_xvbitset_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvbitset_w (_1, _2);
}
v4u64
__lasx_xvbitset_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvbitset_d (_1, _2);
}
v32u8
__lasx_xvbitseti_b (v32u8 _1)
{
  return __builtin_lasx_xvbitseti_b (_1, 1);
}
v16u16
__lasx_xvbitseti_h (v16u16 _1)
{
  return __builtin_lasx_xvbitseti_h (_1, 1);
}
v8u32
__lasx_xvbitseti_w (v8u32 _1)
{
  return __builtin_lasx_xvbitseti_w (_1, 1);
}
v4u64
__lasx_xvbitseti_d (v4u64 _1)
{
  return __builtin_lasx_xvbitseti_d (_1, 1);
}
v32u8
__lasx_xvbitrev_b (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvbitrev_b (_1, _2);
}
v16u16
__lasx_xvbitrev_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvbitrev_h (_1, _2);
}
v8u32
__lasx_xvbitrev_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvbitrev_w (_1, _2);
}
v4u64
__lasx_xvbitrev_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvbitrev_d (_1, _2);
}
v32u8
__lasx_xvbitrevi_b (v32u8 _1)
{
  return __builtin_lasx_xvbitrevi_b (_1, 1);
}
v16u16
__lasx_xvbitrevi_h (v16u16 _1)
{
  return __builtin_lasx_xvbitrevi_h (_1, 1);
}
v8u32
__lasx_xvbitrevi_w (v8u32 _1)
{
  return __builtin_lasx_xvbitrevi_w (_1, 1);
}
v4u64
__lasx_xvbitrevi_d (v4u64 _1)
{
  return __builtin_lasx_xvbitrevi_d (_1, 1);
}
v32i8
__lasx_xvadd_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvadd_b (_1, _2);
}
v16i16
__lasx_xvadd_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvadd_h (_1, _2);
}
v8i32
__lasx_xvadd_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvadd_w (_1, _2);
}
v4i64
__lasx_xvadd_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvadd_d (_1, _2);
}
v32i8
__lasx_xvaddi_bu (v32i8 _1)
{
  return __builtin_lasx_xvaddi_bu (_1, 1);
}
v16i16
__lasx_xvaddi_hu (v16i16 _1)
{
  return __builtin_lasx_xvaddi_hu (_1, 1);
}
v8i32
__lasx_xvaddi_wu (v8i32 _1)
{
  return __builtin_lasx_xvaddi_wu (_1, 1);
}
v4i64
__lasx_xvaddi_du (v4i64 _1)
{
  return __builtin_lasx_xvaddi_du (_1, 1);
}
v32i8
__lasx_xvsub_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsub_b (_1, _2);
}
v16i16
__lasx_xvsub_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsub_h (_1, _2);
}
v8i32
__lasx_xvsub_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsub_w (_1, _2);
}
v4i64
__lasx_xvsub_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsub_d (_1, _2);
}
v32i8
__lasx_xvsubi_bu (v32i8 _1)
{
  return __builtin_lasx_xvsubi_bu (_1, 1);
}
v16i16
__lasx_xvsubi_hu (v16i16 _1)
{
  return __builtin_lasx_xvsubi_hu (_1, 1);
}
v8i32
__lasx_xvsubi_wu (v8i32 _1)
{
  return __builtin_lasx_xvsubi_wu (_1, 1);
}
v4i64
__lasx_xvsubi_du (v4i64 _1)
{
  return __builtin_lasx_xvsubi_du (_1, 1);
}
v32i8
__lasx_xvmax_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmax_b (_1, _2);
}
v16i16
__lasx_xvmax_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmax_h (_1, _2);
}
v8i32
__lasx_xvmax_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmax_w (_1, _2);
}
v4i64
__lasx_xvmax_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmax_d (_1, _2);
}
v32i8
__lasx_xvmaxi_b (v32i8 _1)
{
  return __builtin_lasx_xvmaxi_b (_1, 1);
}
v16i16
__lasx_xvmaxi_h (v16i16 _1)
{
  return __builtin_lasx_xvmaxi_h (_1, 1);
}
v8i32
__lasx_xvmaxi_w (v8i32 _1)
{
  return __builtin_lasx_xvmaxi_w (_1, 1);
}
v4i64
__lasx_xvmaxi_d (v4i64 _1)
{
  return __builtin_lasx_xvmaxi_d (_1, 1);
}
v32u8
__lasx_xvmax_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvmax_bu (_1, _2);
}
v16u16
__lasx_xvmax_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvmax_hu (_1, _2);
}
v8u32
__lasx_xvmax_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvmax_wu (_1, _2);
}
v4u64
__lasx_xvmax_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvmax_du (_1, _2);
}
v32u8
__lasx_xvmaxi_bu (v32u8 _1)
{
  return __builtin_lasx_xvmaxi_bu (_1, 1);
}
v16u16
__lasx_xvmaxi_hu (v16u16 _1)
{
  return __builtin_lasx_xvmaxi_hu (_1, 1);
}
v8u32
__lasx_xvmaxi_wu (v8u32 _1)
{
  return __builtin_lasx_xvmaxi_wu (_1, 1);
}
v4u64
__lasx_xvmaxi_du (v4u64 _1)
{
  return __builtin_lasx_xvmaxi_du (_1, 1);
}
v32i8
__lasx_xvmin_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmin_b (_1, _2);
}
v16i16
__lasx_xvmin_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmin_h (_1, _2);
}
v8i32
__lasx_xvmin_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmin_w (_1, _2);
}
v4i64
__lasx_xvmin_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmin_d (_1, _2);
}
v32i8
__lasx_xvmini_b (v32i8 _1)
{
  return __builtin_lasx_xvmini_b (_1, 1);
}
v16i16
__lasx_xvmini_h (v16i16 _1)
{
  return __builtin_lasx_xvmini_h (_1, 1);
}
v8i32
__lasx_xvmini_w (v8i32 _1)
{
  return __builtin_lasx_xvmini_w (_1, 1);
}
v4i64
__lasx_xvmini_d (v4i64 _1)
{
  return __builtin_lasx_xvmini_d (_1, 1);
}
v32u8
__lasx_xvmin_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvmin_bu (_1, _2);
}
v16u16
__lasx_xvmin_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvmin_hu (_1, _2);
}
v8u32
__lasx_xvmin_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvmin_wu (_1, _2);
}
v4u64
__lasx_xvmin_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvmin_du (_1, _2);
}
v32u8
__lasx_xvmini_bu (v32u8 _1)
{
  return __builtin_lasx_xvmini_bu (_1, 1);
}
v16u16
__lasx_xvmini_hu (v16u16 _1)
{
  return __builtin_lasx_xvmini_hu (_1, 1);
}
v8u32
__lasx_xvmini_wu (v8u32 _1)
{
  return __builtin_lasx_xvmini_wu (_1, 1);
}
v4u64
__lasx_xvmini_du (v4u64 _1)
{
  return __builtin_lasx_xvmini_du (_1, 1);
}
v32i8
__lasx_xvseq_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvseq_b (_1, _2);
}
v16i16
__lasx_xvseq_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvseq_h (_1, _2);
}
v8i32
__lasx_xvseq_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvseq_w (_1, _2);
}
v4i64
__lasx_xvseq_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvseq_d (_1, _2);
}
v32i8
__lasx_xvseqi_b (v32i8 _1)
{
  return __builtin_lasx_xvseqi_b (_1, 1);
}
v16i16
__lasx_xvseqi_h (v16i16 _1)
{
  return __builtin_lasx_xvseqi_h (_1, 1);
}
v8i32
__lasx_xvseqi_w (v8i32 _1)
{
  return __builtin_lasx_xvseqi_w (_1, 1);
}
v4i64
__lasx_xvseqi_d (v4i64 _1)
{
  return __builtin_lasx_xvseqi_d (_1, 1);
}
v32i8
__lasx_xvslt_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvslt_b (_1, _2);
}
v16i16
__lasx_xvslt_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvslt_h (_1, _2);
}
v8i32
__lasx_xvslt_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvslt_w (_1, _2);
}
v4i64
__lasx_xvslt_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvslt_d (_1, _2);
}
v32i8
__lasx_xvslti_b (v32i8 _1)
{
  return __builtin_lasx_xvslti_b (_1, 1);
}
v16i16
__lasx_xvslti_h (v16i16 _1)
{
  return __builtin_lasx_xvslti_h (_1, 1);
}
v8i32
__lasx_xvslti_w (v8i32 _1)
{
  return __builtin_lasx_xvslti_w (_1, 1);
}
v4i64
__lasx_xvslti_d (v4i64 _1)
{
  return __builtin_lasx_xvslti_d (_1, 1);
}
v32i8
__lasx_xvslt_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvslt_bu (_1, _2);
}
v16i16
__lasx_xvslt_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvslt_hu (_1, _2);
}
v8i32
__lasx_xvslt_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvslt_wu (_1, _2);
}
v4i64
__lasx_xvslt_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvslt_du (_1, _2);
}
v32i8
__lasx_xvslti_bu (v32u8 _1)
{
  return __builtin_lasx_xvslti_bu (_1, 1);
}
v16i16
__lasx_xvslti_hu (v16u16 _1)
{
  return __builtin_lasx_xvslti_hu (_1, 1);
}
v8i32
__lasx_xvslti_wu (v8u32 _1)
{
  return __builtin_lasx_xvslti_wu (_1, 1);
}
v4i64
__lasx_xvslti_du (v4u64 _1)
{
  return __builtin_lasx_xvslti_du (_1, 1);
}
v32i8
__lasx_xvsle_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsle_b (_1, _2);
}
v16i16
__lasx_xvsle_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsle_h (_1, _2);
}
v8i32
__lasx_xvsle_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsle_w (_1, _2);
}
v4i64
__lasx_xvsle_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsle_d (_1, _2);
}
v32i8
__lasx_xvslei_b (v32i8 _1)
{
  return __builtin_lasx_xvslei_b (_1, 1);
}
v16i16
__lasx_xvslei_h (v16i16 _1)
{
  return __builtin_lasx_xvslei_h (_1, 1);
}
v8i32
__lasx_xvslei_w (v8i32 _1)
{
  return __builtin_lasx_xvslei_w (_1, 1);
}
v4i64
__lasx_xvslei_d (v4i64 _1)
{
  return __builtin_lasx_xvslei_d (_1, 1);
}
v32i8
__lasx_xvsle_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvsle_bu (_1, _2);
}
v16i16
__lasx_xvsle_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvsle_hu (_1, _2);
}
v8i32
__lasx_xvsle_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvsle_wu (_1, _2);
}
v4i64
__lasx_xvsle_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvsle_du (_1, _2);
}
v32i8
__lasx_xvslei_bu (v32u8 _1)
{
  return __builtin_lasx_xvslei_bu (_1, 1);
}
v16i16
__lasx_xvslei_hu (v16u16 _1)
{
  return __builtin_lasx_xvslei_hu (_1, 1);
}
v8i32
__lasx_xvslei_wu (v8u32 _1)
{
  return __builtin_lasx_xvslei_wu (_1, 1);
}
v4i64
__lasx_xvslei_du (v4u64 _1)
{
  return __builtin_lasx_xvslei_du (_1, 1);
}
v32i8
__lasx_xvsat_b (v32i8 _1)
{
  return __builtin_lasx_xvsat_b (_1, 1);
}
v16i16
__lasx_xvsat_h (v16i16 _1)
{
  return __builtin_lasx_xvsat_h (_1, 1);
}
v8i32
__lasx_xvsat_w (v8i32 _1)
{
  return __builtin_lasx_xvsat_w (_1, 1);
}
v4i64
__lasx_xvsat_d (v4i64 _1)
{
  return __builtin_lasx_xvsat_d (_1, 1);
}
v32u8
__lasx_xvsat_bu (v32u8 _1)
{
  return __builtin_lasx_xvsat_bu (_1, 1);
}
v16u16
__lasx_xvsat_hu (v16u16 _1)
{
  return __builtin_lasx_xvsat_hu (_1, 1);
}
v8u32
__lasx_xvsat_wu (v8u32 _1)
{
  return __builtin_lasx_xvsat_wu (_1, 1);
}
v4u64
__lasx_xvsat_du (v4u64 _1)
{
  return __builtin_lasx_xvsat_du (_1, 1);
}
v32i8
__lasx_xvadda_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvadda_b (_1, _2);
}
v16i16
__lasx_xvadda_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvadda_h (_1, _2);
}
v8i32
__lasx_xvadda_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvadda_w (_1, _2);
}
v4i64
__lasx_xvadda_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvadda_d (_1, _2);
}
v32i8
__lasx_xvsadd_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsadd_b (_1, _2);
}
v16i16
__lasx_xvsadd_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsadd_h (_1, _2);
}
v8i32
__lasx_xvsadd_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsadd_w (_1, _2);
}
v4i64
__lasx_xvsadd_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsadd_d (_1, _2);
}
v32u8
__lasx_xvsadd_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvsadd_bu (_1, _2);
}
v16u16
__lasx_xvsadd_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvsadd_hu (_1, _2);
}
v8u32
__lasx_xvsadd_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvsadd_wu (_1, _2);
}
v4u64
__lasx_xvsadd_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvsadd_du (_1, _2);
}
v32i8
__lasx_xvavg_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvavg_b (_1, _2);
}
v16i16
__lasx_xvavg_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvavg_h (_1, _2);
}
v8i32
__lasx_xvavg_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvavg_w (_1, _2);
}
v4i64
__lasx_xvavg_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvavg_d (_1, _2);
}
v32u8
__lasx_xvavg_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvavg_bu (_1, _2);
}
v16u16
__lasx_xvavg_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvavg_hu (_1, _2);
}
v8u32
__lasx_xvavg_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvavg_wu (_1, _2);
}
v4u64
__lasx_xvavg_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvavg_du (_1, _2);
}
v32i8
__lasx_xvavgr_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvavgr_b (_1, _2);
}
v16i16
__lasx_xvavgr_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvavgr_h (_1, _2);
}
v8i32
__lasx_xvavgr_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvavgr_w (_1, _2);
}
v4i64
__lasx_xvavgr_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvavgr_d (_1, _2);
}
v32u8
__lasx_xvavgr_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvavgr_bu (_1, _2);
}
v16u16
__lasx_xvavgr_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvavgr_hu (_1, _2);
}
v8u32
__lasx_xvavgr_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvavgr_wu (_1, _2);
}
v4u64
__lasx_xvavgr_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvavgr_du (_1, _2);
}
v32i8
__lasx_xvssub_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssub_b (_1, _2);
}
v16i16
__lasx_xvssub_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssub_h (_1, _2);
}
v8i32
__lasx_xvssub_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssub_w (_1, _2);
}
v4i64
__lasx_xvssub_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssub_d (_1, _2);
}
v32u8
__lasx_xvssub_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvssub_bu (_1, _2);
}
v16u16
__lasx_xvssub_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvssub_hu (_1, _2);
}
v8u32
__lasx_xvssub_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvssub_wu (_1, _2);
}
v4u64
__lasx_xvssub_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvssub_du (_1, _2);
}
v32i8
__lasx_xvabsd_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvabsd_b (_1, _2);
}
v16i16
__lasx_xvabsd_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvabsd_h (_1, _2);
}
v8i32
__lasx_xvabsd_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvabsd_w (_1, _2);
}
v4i64
__lasx_xvabsd_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvabsd_d (_1, _2);
}
v32u8
__lasx_xvabsd_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvabsd_bu (_1, _2);
}
v16u16
__lasx_xvabsd_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvabsd_hu (_1, _2);
}
v8u32
__lasx_xvabsd_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvabsd_wu (_1, _2);
}
v4u64
__lasx_xvabsd_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvabsd_du (_1, _2);
}
v32i8
__lasx_xvmul_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmul_b (_1, _2);
}
v16i16
__lasx_xvmul_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmul_h (_1, _2);
}
v8i32
__lasx_xvmul_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmul_w (_1, _2);
}
v4i64
__lasx_xvmul_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmul_d (_1, _2);
}
v32i8
__lasx_xvmadd_b (v32i8 _1, v32i8 _2, v32i8 _3)
{
  return __builtin_lasx_xvmadd_b (_1, _2, _3);
}
v16i16
__lasx_xvmadd_h (v16i16 _1, v16i16 _2, v16i16 _3)
{
  return __builtin_lasx_xvmadd_h (_1, _2, _3);
}
v8i32
__lasx_xvmadd_w (v8i32 _1, v8i32 _2, v8i32 _3)
{
  return __builtin_lasx_xvmadd_w (_1, _2, _3);
}
v4i64
__lasx_xvmadd_d (v4i64 _1, v4i64 _2, v4i64 _3)
{
  return __builtin_lasx_xvmadd_d (_1, _2, _3);
}
v32i8
__lasx_xvmsub_b (v32i8 _1, v32i8 _2, v32i8 _3)
{
  return __builtin_lasx_xvmsub_b (_1, _2, _3);
}
v16i16
__lasx_xvmsub_h (v16i16 _1, v16i16 _2, v16i16 _3)
{
  return __builtin_lasx_xvmsub_h (_1, _2, _3);
}
v8i32
__lasx_xvmsub_w (v8i32 _1, v8i32 _2, v8i32 _3)
{
  return __builtin_lasx_xvmsub_w (_1, _2, _3);
}
v4i64
__lasx_xvmsub_d (v4i64 _1, v4i64 _2, v4i64 _3)
{
  return __builtin_lasx_xvmsub_d (_1, _2, _3);
}
v32i8
__lasx_xvdiv_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvdiv_b (_1, _2);
}
v16i16
__lasx_xvdiv_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvdiv_h (_1, _2);
}
v8i32
__lasx_xvdiv_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvdiv_w (_1, _2);
}
v4i64
__lasx_xvdiv_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvdiv_d (_1, _2);
}
v32u8
__lasx_xvdiv_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvdiv_bu (_1, _2);
}
v16u16
__lasx_xvdiv_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvdiv_hu (_1, _2);
}
v8u32
__lasx_xvdiv_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvdiv_wu (_1, _2);
}
v4u64
__lasx_xvdiv_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvdiv_du (_1, _2);
}
v16i16
__lasx_xvhaddw_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvhaddw_h_b (_1, _2);
}
v8i32
__lasx_xvhaddw_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvhaddw_w_h (_1, _2);
}
v4i64
__lasx_xvhaddw_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvhaddw_d_w (_1, _2);
}
v16u16
__lasx_xvhaddw_hu_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvhaddw_hu_bu (_1, _2);
}
v8u32
__lasx_xvhaddw_wu_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvhaddw_wu_hu (_1, _2);
}
v4u64
__lasx_xvhaddw_du_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvhaddw_du_wu (_1, _2);
}
v16i16
__lasx_xvhsubw_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvhsubw_h_b (_1, _2);
}
v8i32
__lasx_xvhsubw_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvhsubw_w_h (_1, _2);
}
v4i64
__lasx_xvhsubw_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvhsubw_d_w (_1, _2);
}
v16i16
__lasx_xvhsubw_hu_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvhsubw_hu_bu (_1, _2);
}
v8i32
__lasx_xvhsubw_wu_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvhsubw_wu_hu (_1, _2);
}
v4i64
__lasx_xvhsubw_du_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvhsubw_du_wu (_1, _2);
}
v32i8
__lasx_xvmod_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmod_b (_1, _2);
}
v16i16
__lasx_xvmod_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmod_h (_1, _2);
}
v8i32
__lasx_xvmod_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmod_w (_1, _2);
}
v4i64
__lasx_xvmod_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmod_d (_1, _2);
}
v32u8
__lasx_xvmod_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvmod_bu (_1, _2);
}
v16u16
__lasx_xvmod_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvmod_hu (_1, _2);
}
v8u32
__lasx_xvmod_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvmod_wu (_1, _2);
}
v4u64
__lasx_xvmod_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvmod_du (_1, _2);
}
v32i8
__lasx_xvrepl128vei_b (v32i8 _1)
{
  return __builtin_lasx_xvrepl128vei_b (_1, 1);
}
v16i16
__lasx_xvrepl128vei_h (v16i16 _1)
{
  return __builtin_lasx_xvrepl128vei_h (_1, 1);
}
v8i32
__lasx_xvrepl128vei_w (v8i32 _1)
{
  return __builtin_lasx_xvrepl128vei_w (_1, 1);
}
v4i64
__lasx_xvrepl128vei_d (v4i64 _1)
{
  return __builtin_lasx_xvrepl128vei_d (_1, 1);
}
v32i8
__lasx_xvpickev_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvpickev_b (_1, _2);
}
v16i16
__lasx_xvpickev_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvpickev_h (_1, _2);
}
v8i32
__lasx_xvpickev_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvpickev_w (_1, _2);
}
v4i64
__lasx_xvpickev_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvpickev_d (_1, _2);
}
v32i8
__lasx_xvpickod_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvpickod_b (_1, _2);
}
v16i16
__lasx_xvpickod_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvpickod_h (_1, _2);
}
v8i32
__lasx_xvpickod_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvpickod_w (_1, _2);
}
v4i64
__lasx_xvpickod_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvpickod_d (_1, _2);
}
v32i8
__lasx_xvilvh_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvilvh_b (_1, _2);
}
v16i16
__lasx_xvilvh_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvilvh_h (_1, _2);
}
v8i32
__lasx_xvilvh_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvilvh_w (_1, _2);
}
v4i64
__lasx_xvilvh_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvilvh_d (_1, _2);
}
v32i8
__lasx_xvilvl_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvilvl_b (_1, _2);
}
v16i16
__lasx_xvilvl_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvilvl_h (_1, _2);
}
v8i32
__lasx_xvilvl_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvilvl_w (_1, _2);
}
v4i64
__lasx_xvilvl_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvilvl_d (_1, _2);
}
v32i8
__lasx_xvpackev_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvpackev_b (_1, _2);
}
v16i16
__lasx_xvpackev_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvpackev_h (_1, _2);
}
v8i32
__lasx_xvpackev_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvpackev_w (_1, _2);
}
v4i64
__lasx_xvpackev_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvpackev_d (_1, _2);
}
v32i8
__lasx_xvpackod_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvpackod_b (_1, _2);
}
v16i16
__lasx_xvpackod_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvpackod_h (_1, _2);
}
v8i32
__lasx_xvpackod_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvpackod_w (_1, _2);
}
v4i64
__lasx_xvpackod_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvpackod_d (_1, _2);
}
v32i8
__lasx_xvshuf_b (v32i8 _1, v32i8 _2, v32i8 _3)
{
  return __builtin_lasx_xvshuf_b (_1, _2, _3);
}
v16i16
__lasx_xvshuf_h (v16i16 _1, v16i16 _2, v16i16 _3)
{
  return __builtin_lasx_xvshuf_h (_1, _2, _3);
}
v8i32
__lasx_xvshuf_w (v8i32 _1, v8i32 _2, v8i32 _3)
{
  return __builtin_lasx_xvshuf_w (_1, _2, _3);
}
v4i64
__lasx_xvshuf_d (v4i64 _1, v4i64 _2, v4i64 _3)
{
  return __builtin_lasx_xvshuf_d (_1, _2, _3);
}
v32u8
__lasx_xvand_v (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvand_v (_1, _2);
}
v32u8
__lasx_xvandi_b (v32u8 _1)
{
  return __builtin_lasx_xvandi_b (_1, 1);
}
v32u8
__lasx_xvor_v (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvor_v (_1, _2);
}
v32u8
__lasx_xvori_b (v32u8 _1)
{
  return __builtin_lasx_xvori_b (_1, 1);
}
v32u8
__lasx_xvnor_v (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvnor_v (_1, _2);
}
v32u8
__lasx_xvnori_b (v32u8 _1)
{
  return __builtin_lasx_xvnori_b (_1, 1);
}
v32u8
__lasx_xvxor_v (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvxor_v (_1, _2);
}
v32u8
__lasx_xvxori_b (v32u8 _1)
{
  return __builtin_lasx_xvxori_b (_1, 1);
}
v32u8
__lasx_xvbitsel_v (v32u8 _1, v32u8 _2, v32u8 _3)
{
  return __builtin_lasx_xvbitsel_v (_1, _2, _3);
}
v32u8
__lasx_xvbitseli_b (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvbitseli_b (_1, _2, 1);
}
v32i8
__lasx_xvshuf4i_b (v32i8 _1)
{
  return __builtin_lasx_xvshuf4i_b (_1, 1);
}
v16i16
__lasx_xvshuf4i_h (v16i16 _1)
{
  return __builtin_lasx_xvshuf4i_h (_1, 1);
}
v8i32
__lasx_xvshuf4i_w (v8i32 _1)
{
  return __builtin_lasx_xvshuf4i_w (_1, 1);
}
v32i8
__lasx_xvreplgr2vr_b (int _1)
{
  return __builtin_lasx_xvreplgr2vr_b (_1);
}
v16i16
__lasx_xvreplgr2vr_h (int _1)
{
  return __builtin_lasx_xvreplgr2vr_h (_1);
}
v8i32
__lasx_xvreplgr2vr_w (int _1)
{
  return __builtin_lasx_xvreplgr2vr_w (_1);
}
v4i64
__lasx_xvreplgr2vr_d (int _1)
{
  return __builtin_lasx_xvreplgr2vr_d (_1);
}
v32i8
__lasx_xvpcnt_b (v32i8 _1)
{
  return __builtin_lasx_xvpcnt_b (_1);
}
v16i16
__lasx_xvpcnt_h (v16i16 _1)
{
  return __builtin_lasx_xvpcnt_h (_1);
}
v8i32
__lasx_xvpcnt_w (v8i32 _1)
{
  return __builtin_lasx_xvpcnt_w (_1);
}
v4i64
__lasx_xvpcnt_d (v4i64 _1)
{
  return __builtin_lasx_xvpcnt_d (_1);
}
v32i8
__lasx_xvclo_b (v32i8 _1)
{
  return __builtin_lasx_xvclo_b (_1);
}
v16i16
__lasx_xvclo_h (v16i16 _1)
{
  return __builtin_lasx_xvclo_h (_1);
}
v8i32
__lasx_xvclo_w (v8i32 _1)
{
  return __builtin_lasx_xvclo_w (_1);
}
v4i64
__lasx_xvclo_d (v4i64 _1)
{
  return __builtin_lasx_xvclo_d (_1);
}
v32i8
__lasx_xvclz_b (v32i8 _1)
{
  return __builtin_lasx_xvclz_b (_1);
}
v16i16
__lasx_xvclz_h (v16i16 _1)
{
  return __builtin_lasx_xvclz_h (_1);
}
v8i32
__lasx_xvclz_w (v8i32 _1)
{
  return __builtin_lasx_xvclz_w (_1);
}
v4i64
__lasx_xvclz_d (v4i64 _1)
{
  return __builtin_lasx_xvclz_d (_1);
}
v8f32
__lasx_xvfadd_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfadd_s (_1, _2);
}
v4f64
__lasx_xvfadd_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfadd_d (_1, _2);
}
v8f32
__lasx_xvfsub_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfsub_s (_1, _2);
}
v4f64
__lasx_xvfsub_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfsub_d (_1, _2);
}
v8f32
__lasx_xvfmul_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfmul_s (_1, _2);
}
v4f64
__lasx_xvfmul_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfmul_d (_1, _2);
}
v8f32
__lasx_xvfdiv_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfdiv_s (_1, _2);
}
v4f64
__lasx_xvfdiv_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfdiv_d (_1, _2);
}
v16i16
__lasx_xvfcvt_h_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcvt_h_s (_1, _2);
}
v8f32
__lasx_xvfcvt_s_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcvt_s_d (_1, _2);
}
v8f32
__lasx_xvfmin_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfmin_s (_1, _2);
}
v4f64
__lasx_xvfmin_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfmin_d (_1, _2);
}
v8f32
__lasx_xvfmina_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfmina_s (_1, _2);
}
v4f64
__lasx_xvfmina_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfmina_d (_1, _2);
}
v8f32
__lasx_xvfmax_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfmax_s (_1, _2);
}
v4f64
__lasx_xvfmax_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfmax_d (_1, _2);
}
v8f32
__lasx_xvfmaxa_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfmaxa_s (_1, _2);
}
v4f64
__lasx_xvfmaxa_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfmaxa_d (_1, _2);
}
v8i32
__lasx_xvfclass_s (v8f32 _1)
{
  return __builtin_lasx_xvfclass_s (_1);
}
v4i64
__lasx_xvfclass_d (v4f64 _1)
{
  return __builtin_lasx_xvfclass_d (_1);
}
v8f32
__lasx_xvfsqrt_s (v8f32 _1)
{
  return __builtin_lasx_xvfsqrt_s (_1);
}
v4f64
__lasx_xvfsqrt_d (v4f64 _1)
{
  return __builtin_lasx_xvfsqrt_d (_1);
}
v8f32
__lasx_xvfrecip_s (v8f32 _1)
{
  return __builtin_lasx_xvfrecip_s (_1);
}
v4f64
__lasx_xvfrecip_d (v4f64 _1)
{
  return __builtin_lasx_xvfrecip_d (_1);
}
v8f32
__lasx_xvfrint_s (v8f32 _1)
{
  return __builtin_lasx_xvfrint_s (_1);
}
v4f64
__lasx_xvfrint_d (v4f64 _1)
{
  return __builtin_lasx_xvfrint_d (_1);
}
v8f32
__lasx_xvfrsqrt_s (v8f32 _1)
{
  return __builtin_lasx_xvfrsqrt_s (_1);
}
v4f64
__lasx_xvfrsqrt_d (v4f64 _1)
{
  return __builtin_lasx_xvfrsqrt_d (_1);
}
v8f32
__lasx_xvflogb_s (v8f32 _1)
{
  return __builtin_lasx_xvflogb_s (_1);
}
v4f64
__lasx_xvflogb_d (v4f64 _1)
{
  return __builtin_lasx_xvflogb_d (_1);
}
v8f32
__lasx_xvfcvth_s_h (v16i16 _1)
{
  return __builtin_lasx_xvfcvth_s_h (_1);
}
v4f64
__lasx_xvfcvth_d_s (v8f32 _1)
{
  return __builtin_lasx_xvfcvth_d_s (_1);
}
v8f32
__lasx_xvfcvtl_s_h (v16i16 _1)
{
  return __builtin_lasx_xvfcvtl_s_h (_1);
}
v4f64
__lasx_xvfcvtl_d_s (v8f32 _1)
{
  return __builtin_lasx_xvfcvtl_d_s (_1);
}
v8i32
__lasx_xvftint_w_s (v8f32 _1)
{
  return __builtin_lasx_xvftint_w_s (_1);
}
v4i64
__lasx_xvftint_l_d (v4f64 _1)
{
  return __builtin_lasx_xvftint_l_d (_1);
}
v8u32
__lasx_xvftint_wu_s (v8f32 _1)
{
  return __builtin_lasx_xvftint_wu_s (_1);
}
v4u64
__lasx_xvftint_lu_d (v4f64 _1)
{
  return __builtin_lasx_xvftint_lu_d (_1);
}
v8i32
__lasx_xvftintrz_w_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrz_w_s (_1);
}
v4i64
__lasx_xvftintrz_l_d (v4f64 _1)
{
  return __builtin_lasx_xvftintrz_l_d (_1);
}
v8u32
__lasx_xvftintrz_wu_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrz_wu_s (_1);
}
v4u64
__lasx_xvftintrz_lu_d (v4f64 _1)
{
  return __builtin_lasx_xvftintrz_lu_d (_1);
}
v8f32
__lasx_xvffint_s_w (v8i32 _1)
{
  return __builtin_lasx_xvffint_s_w (_1);
}
v4f64
__lasx_xvffint_d_l (v4i64 _1)
{
  return __builtin_lasx_xvffint_d_l (_1);
}
v8f32
__lasx_xvffint_s_wu (v8u32 _1)
{
  return __builtin_lasx_xvffint_s_wu (_1);
}
v4f64
__lasx_xvffint_d_lu (v4u64 _1)
{
  return __builtin_lasx_xvffint_d_lu (_1);
}
v32i8
__lasx_xvreplve_b (v32i8 _1, int _2)
{
  return __builtin_lasx_xvreplve_b (_1, _2);
}
v16i16
__lasx_xvreplve_h (v16i16 _1, int _2)
{
  return __builtin_lasx_xvreplve_h (_1, _2);
}
v8i32
__lasx_xvreplve_w (v8i32 _1, int _2)
{
  return __builtin_lasx_xvreplve_w (_1, _2);
}
v4i64
__lasx_xvreplve_d (v4i64 _1, int _2)
{
  return __builtin_lasx_xvreplve_d (_1, _2);
}
v8i32
__lasx_xvpermi_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvpermi_w (_1, _2, 1);
}
v32u8
__lasx_xvandn_v (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvandn_v (_1, _2);
}
v32i8
__lasx_xvneg_b (v32i8 _1)
{
  return __builtin_lasx_xvneg_b (_1);
}
v16i16
__lasx_xvneg_h (v16i16 _1)
{
  return __builtin_lasx_xvneg_h (_1);
}
v8i32
__lasx_xvneg_w (v8i32 _1)
{
  return __builtin_lasx_xvneg_w (_1);
}
v4i64
__lasx_xvneg_d (v4i64 _1)
{
  return __builtin_lasx_xvneg_d (_1);
}
v32i8
__lasx_xvmuh_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmuh_b (_1, _2);
}
v16i16
__lasx_xvmuh_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmuh_h (_1, _2);
}
v8i32
__lasx_xvmuh_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmuh_w (_1, _2);
}
v4i64
__lasx_xvmuh_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmuh_d (_1, _2);
}
v32u8
__lasx_xvmuh_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvmuh_bu (_1, _2);
}
v16u16
__lasx_xvmuh_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvmuh_hu (_1, _2);
}
v8u32
__lasx_xvmuh_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvmuh_wu (_1, _2);
}
v4u64
__lasx_xvmuh_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvmuh_du (_1, _2);
}
v16i16
__lasx_xvsllwil_h_b (v32i8 _1)
{
  return __builtin_lasx_xvsllwil_h_b (_1, 1);
}
v8i32
__lasx_xvsllwil_w_h (v16i16 _1)
{
  return __builtin_lasx_xvsllwil_w_h (_1, 1);
}
v4i64
__lasx_xvsllwil_d_w (v8i32 _1)
{
  return __builtin_lasx_xvsllwil_d_w (_1, 1);
}
v16u16
__lasx_xvsllwil_hu_bu (v32u8 _1)
{
  return __builtin_lasx_xvsllwil_hu_bu (_1, 1);
}
v8u32
__lasx_xvsllwil_wu_hu (v16u16 _1)
{
  return __builtin_lasx_xvsllwil_wu_hu (_1, 1);
}
v4u64
__lasx_xvsllwil_du_wu (v8u32 _1)
{
  return __builtin_lasx_xvsllwil_du_wu (_1, 1);
}
v32i8
__lasx_xvsran_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsran_b_h (_1, _2);
}
v16i16
__lasx_xvsran_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsran_h_w (_1, _2);
}
v8i32
__lasx_xvsran_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsran_w_d (_1, _2);
}
v32i8
__lasx_xvssran_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssran_b_h (_1, _2);
}
v16i16
__lasx_xvssran_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssran_h_w (_1, _2);
}
v8i32
__lasx_xvssran_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssran_w_d (_1, _2);
}
v32u8
__lasx_xvssran_bu_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvssran_bu_h (_1, _2);
}
v16u16
__lasx_xvssran_hu_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvssran_hu_w (_1, _2);
}
v8u32
__lasx_xvssran_wu_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvssran_wu_d (_1, _2);
}
v32i8
__lasx_xvsrarn_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrarn_b_h (_1, _2);
}
v16i16
__lasx_xvsrarn_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrarn_h_w (_1, _2);
}
v8i32
__lasx_xvsrarn_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrarn_w_d (_1, _2);
}
v32i8
__lasx_xvssrarn_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrarn_b_h (_1, _2);
}
v16i16
__lasx_xvssrarn_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrarn_h_w (_1, _2);
}
v8i32
__lasx_xvssrarn_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrarn_w_d (_1, _2);
}
v32u8
__lasx_xvssrarn_bu_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvssrarn_bu_h (_1, _2);
}
v16u16
__lasx_xvssrarn_hu_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvssrarn_hu_w (_1, _2);
}
v8u32
__lasx_xvssrarn_wu_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvssrarn_wu_d (_1, _2);
}
v32i8
__lasx_xvsrln_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrln_b_h (_1, _2);
}
v16i16
__lasx_xvsrln_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrln_h_w (_1, _2);
}
v8i32
__lasx_xvsrln_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrln_w_d (_1, _2);
}
v32u8
__lasx_xvssrln_bu_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvssrln_bu_h (_1, _2);
}
v16u16
__lasx_xvssrln_hu_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvssrln_hu_w (_1, _2);
}
v8u32
__lasx_xvssrln_wu_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvssrln_wu_d (_1, _2);
}
v32i8
__lasx_xvsrlrn_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrlrn_b_h (_1, _2);
}
v16i16
__lasx_xvsrlrn_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrlrn_h_w (_1, _2);
}
v8i32
__lasx_xvsrlrn_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrlrn_w_d (_1, _2);
}
v32u8
__lasx_xvssrlrn_bu_h (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvssrlrn_bu_h (_1, _2);
}
v16u16
__lasx_xvssrlrn_hu_w (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvssrlrn_hu_w (_1, _2);
}
v8u32
__lasx_xvssrlrn_wu_d (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvssrlrn_wu_d (_1, _2);
}
v32i8
__lasx_xvfrstpi_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvfrstpi_b (_1, _2, 1);
}
v16i16
__lasx_xvfrstpi_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvfrstpi_h (_1, _2, 1);
}
v32i8
__lasx_xvfrstp_b (v32i8 _1, v32i8 _2, v32i8 _3)
{
  return __builtin_lasx_xvfrstp_b (_1, _2, _3);
}
v16i16
__lasx_xvfrstp_h (v16i16 _1, v16i16 _2, v16i16 _3)
{
  return __builtin_lasx_xvfrstp_h (_1, _2, _3);
}
v4i64
__lasx_xvshuf4i_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvshuf4i_d (_1, _2, 1);
}
v32i8
__lasx_xvbsrl_v (v32i8 _1)
{
  return __builtin_lasx_xvbsrl_v (_1, 1);
}
v32i8
__lasx_xvbsll_v (v32i8 _1)
{
  return __builtin_lasx_xvbsll_v (_1, 1);
}
v32i8
__lasx_xvextrins_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvextrins_b (_1, _2, 1);
}
v16i16
__lasx_xvextrins_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvextrins_h (_1, _2, 1);
}
v8i32
__lasx_xvextrins_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvextrins_w (_1, _2, 1);
}
v4i64
__lasx_xvextrins_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvextrins_d (_1, _2, 1);
}
v32i8
__lasx_xvmskltz_b (v32i8 _1)
{
  return __builtin_lasx_xvmskltz_b (_1);
}
v16i16
__lasx_xvmskltz_h (v16i16 _1)
{
  return __builtin_lasx_xvmskltz_h (_1);
}
v8i32
__lasx_xvmskltz_w (v8i32 _1)
{
  return __builtin_lasx_xvmskltz_w (_1);
}
v4i64
__lasx_xvmskltz_d (v4i64 _1)
{
  return __builtin_lasx_xvmskltz_d (_1);
}
v32i8
__lasx_xvsigncov_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsigncov_b (_1, _2);
}
v16i16
__lasx_xvsigncov_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsigncov_h (_1, _2);
}
v8i32
__lasx_xvsigncov_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsigncov_w (_1, _2);
}
v4i64
__lasx_xvsigncov_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsigncov_d (_1, _2);
}
v8f32
__lasx_xvfmadd_s (v8f32 _1, v8f32 _2, v8f32 _3)
{
  return __builtin_lasx_xvfmadd_s (_1, _2, _3);
}
v4f64
__lasx_xvfmadd_d (v4f64 _1, v4f64 _2, v4f64 _3)
{
  return __builtin_lasx_xvfmadd_d (_1, _2, _3);
}
v8f32
__lasx_xvfmsub_s (v8f32 _1, v8f32 _2, v8f32 _3)
{
  return __builtin_lasx_xvfmsub_s (_1, _2, _3);
}
v4f64
__lasx_xvfmsub_d (v4f64 _1, v4f64 _2, v4f64 _3)
{
  return __builtin_lasx_xvfmsub_d (_1, _2, _3);
}
v8f32
__lasx_xvfnmadd_s (v8f32 _1, v8f32 _2, v8f32 _3)
{
  return __builtin_lasx_xvfnmadd_s (_1, _2, _3);
}
v4f64
__lasx_xvfnmadd_d (v4f64 _1, v4f64 _2, v4f64 _3)
{
  return __builtin_lasx_xvfnmadd_d (_1, _2, _3);
}
v8f32
__lasx_xvfnmsub_s (v8f32 _1, v8f32 _2, v8f32 _3)
{
  return __builtin_lasx_xvfnmsub_s (_1, _2, _3);
}
v4f64
__lasx_xvfnmsub_d (v4f64 _1, v4f64 _2, v4f64 _3)
{
  return __builtin_lasx_xvfnmsub_d (_1, _2, _3);
}
v8i32
__lasx_xvftintrne_w_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrne_w_s (_1);
}
v4i64
__lasx_xvftintrne_l_d (v4f64 _1)
{
  return __builtin_lasx_xvftintrne_l_d (_1);
}
v8i32
__lasx_xvftintrp_w_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrp_w_s (_1);
}
v4i64
__lasx_xvftintrp_l_d (v4f64 _1)
{
  return __builtin_lasx_xvftintrp_l_d (_1);
}
v8i32
__lasx_xvftintrm_w_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrm_w_s (_1);
}
v4i64
__lasx_xvftintrm_l_d (v4f64 _1)
{
  return __builtin_lasx_xvftintrm_l_d (_1);
}
v8i32
__lasx_xvftint_w_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvftint_w_d (_1, _2);
}
v8f32
__lasx_xvffint_s_l (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvffint_s_l (_1, _2);
}
v8i32
__lasx_xvftintrz_w_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvftintrz_w_d (_1, _2);
}
v8i32
__lasx_xvftintrp_w_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvftintrp_w_d (_1, _2);
}
v8i32
__lasx_xvftintrm_w_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvftintrm_w_d (_1, _2);
}
v8i32
__lasx_xvftintrne_w_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvftintrne_w_d (_1, _2);
}
v4i64
__lasx_xvftinth_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftinth_l_s (_1);
}
v4i64
__lasx_xvftintl_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintl_l_s (_1);
}
v4f64
__lasx_xvffinth_d_w (v8i32 _1)
{
  return __builtin_lasx_xvffinth_d_w (_1);
}
v4f64
__lasx_xvffintl_d_w (v8i32 _1)
{
  return __builtin_lasx_xvffintl_d_w (_1);
}
v4i64
__lasx_xvftintrzh_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrzh_l_s (_1);
}
v4i64
__lasx_xvftintrzl_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrzl_l_s (_1);
}
v4i64
__lasx_xvftintrph_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrph_l_s (_1);
}
v4i64
__lasx_xvftintrpl_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrpl_l_s (_1);
}
v4i64
__lasx_xvftintrmh_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrmh_l_s (_1);
}
v4i64
__lasx_xvftintrml_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrml_l_s (_1);
}
v4i64
__lasx_xvftintrneh_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrneh_l_s (_1);
}
v4i64
__lasx_xvftintrnel_l_s (v8f32 _1)
{
  return __builtin_lasx_xvftintrnel_l_s (_1);
}
v8f32
__lasx_xvfrintrne_s (v8f32 _1)
{
  return __builtin_lasx_xvfrintrne_s (_1);
}
v4f64
__lasx_xvfrintrne_d (v4f64 _1)
{
  return __builtin_lasx_xvfrintrne_d (_1);
}
v8f32
__lasx_xvfrintrz_s (v8f32 _1)
{
  return __builtin_lasx_xvfrintrz_s (_1);
}
v4f64
__lasx_xvfrintrz_d (v4f64 _1)
{
  return __builtin_lasx_xvfrintrz_d (_1);
}
v8f32
__lasx_xvfrintrp_s (v8f32 _1)
{
  return __builtin_lasx_xvfrintrp_s (_1);
}
v4f64
__lasx_xvfrintrp_d (v4f64 _1)
{
  return __builtin_lasx_xvfrintrp_d (_1);
}
v8f32
__lasx_xvfrintrm_s (v8f32 _1)
{
  return __builtin_lasx_xvfrintrm_s (_1);
}
v4f64
__lasx_xvfrintrm_d (v4f64 _1)
{
  return __builtin_lasx_xvfrintrm_d (_1);
}
v32i8
__lasx_xvld (void *_1)
{
  return __builtin_lasx_xvld (_1, 1);
}
void
__lasx_xvst (v32i8 _1, void *_2)
{
  return __builtin_lasx_xvst (_1, _2, 1);
}
void
__lasx_xvstelm_b (v32i8 _1, void *_2)
{
  return __builtin_lasx_xvstelm_b (_1, _2, 1, 1);
}
void
__lasx_xvstelm_h (v16i16 _1, void *_2)
{
  return __builtin_lasx_xvstelm_h (_1, _2, 2, 1);
}
void
__lasx_xvstelm_w (v8i32 _1, void *_2)
{
  return __builtin_lasx_xvstelm_w (_1, _2, 4, 1);
}
void
__lasx_xvstelm_d (v4i64 _1, void *_2)
{
  return __builtin_lasx_xvstelm_d (_1, _2, 8, 1);
}
v8i32
__lasx_xvinsve0_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvinsve0_w (_1, _2, 1);
}
v4i64
__lasx_xvinsve0_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvinsve0_d (_1, _2, 1);
}
v8i32
__lasx_xvpickve_w (v8i32 _1)
{
  return __builtin_lasx_xvpickve_w (_1, 1);
}
v4i64
__lasx_xvpickve_d (v4i64 _1)
{
  return __builtin_lasx_xvpickve_d (_1, 1);
}
v32i8
__lasx_xvssrlrn_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrlrn_b_h (_1, _2);
}
v16i16
__lasx_xvssrlrn_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrlrn_h_w (_1, _2);
}
v8i32
__lasx_xvssrlrn_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrlrn_w_d (_1, _2);
}
v32i8
__lasx_xvssrln_b_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrln_b_h (_1, _2);
}
v16i16
__lasx_xvssrln_h_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrln_h_w (_1, _2);
}
v8i32
__lasx_xvssrln_w_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrln_w_d (_1, _2);
}
v32u8
__lasx_xvorn_v (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvorn_v (_1, _2);
}
v4i64
__lasx_xvldi ()
{
  return __builtin_lasx_xvldi (1);
}
v32i8
__lasx_xvldx (void *_1)
{
  return __builtin_lasx_xvldx (_1, 1);
}
void
__lasx_xvstx (v32i8 _1, void *_2)
{
  return __builtin_lasx_xvstx (_1, _2, 1);
}
v4u64
__lasx_xvextl_qu_du (v4u64 _1)
{
  return __builtin_lasx_xvextl_qu_du (_1);
}
v8i32
__lasx_xvinsgr2vr_w (v8i32 _1)
{
  return __builtin_lasx_xvinsgr2vr_w (_1, 1, 1);
}
v4i64
__lasx_xvinsgr2vr_d (v4i64 _1)
{
  return __builtin_lasx_xvinsgr2vr_d (_1, 1, 1);
}
v32i8
__lasx_xvreplve0_b (v32i8 _1)
{
  return __builtin_lasx_xvreplve0_b (_1);
}
v16i16
__lasx_xvreplve0_h (v16i16 _1)
{
  return __builtin_lasx_xvreplve0_h (_1);
}
v8i32
__lasx_xvreplve0_w (v8i32 _1)
{
  return __builtin_lasx_xvreplve0_w (_1);
}
v4i64
__lasx_xvreplve0_d (v4i64 _1)
{
  return __builtin_lasx_xvreplve0_d (_1);
}
v32i8
__lasx_xvreplve0_q (v32i8 _1)
{
  return __builtin_lasx_xvreplve0_q (_1);
}
v16i16
__lasx_vext2xv_h_b (v32i8 _1)
{
  return __builtin_lasx_vext2xv_h_b (_1);
}
v8i32
__lasx_vext2xv_w_h (v16i16 _1)
{
  return __builtin_lasx_vext2xv_w_h (_1);
}
v4i64
__lasx_vext2xv_d_w (v8i32 _1)
{
  return __builtin_lasx_vext2xv_d_w (_1);
}
v8i32
__lasx_vext2xv_w_b (v32i8 _1)
{
  return __builtin_lasx_vext2xv_w_b (_1);
}
v4i64
__lasx_vext2xv_d_h (v16i16 _1)
{
  return __builtin_lasx_vext2xv_d_h (_1);
}
v4i64
__lasx_vext2xv_d_b (v32i8 _1)
{
  return __builtin_lasx_vext2xv_d_b (_1);
}
v16i16
__lasx_vext2xv_hu_bu (v32i8 _1)
{
  return __builtin_lasx_vext2xv_hu_bu (_1);
}
v8i32
__lasx_vext2xv_wu_hu (v16i16 _1)
{
  return __builtin_lasx_vext2xv_wu_hu (_1);
}
v4i64
__lasx_vext2xv_du_wu (v8i32 _1)
{
  return __builtin_lasx_vext2xv_du_wu (_1);
}
v8i32
__lasx_vext2xv_wu_bu (v32i8 _1)
{
  return __builtin_lasx_vext2xv_wu_bu (_1);
}
v4i64
__lasx_vext2xv_du_hu (v16i16 _1)
{
  return __builtin_lasx_vext2xv_du_hu (_1);
}
v4i64
__lasx_vext2xv_du_bu (v32i8 _1)
{
  return __builtin_lasx_vext2xv_du_bu (_1);
}
v32i8
__lasx_xvpermi_q (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvpermi_q (_1, _2, 1);
}
v4i64
__lasx_xvpermi_d (v4i64 _1)
{
  return __builtin_lasx_xvpermi_d (_1, 1);
}
v8i32
__lasx_xvperm_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvperm_w (_1, _2);
}
v32i8
__lasx_xvldrepl_b (void *_1)
{
  return __builtin_lasx_xvldrepl_b (_1, 1);
}
v16i16
__lasx_xvldrepl_h (void *_1)
{
  return __builtin_lasx_xvldrepl_h (_1, 2);
}
v8i32
__lasx_xvldrepl_w (void *_1)
{
  return __builtin_lasx_xvldrepl_w (_1, 4);
}
v4i64
__lasx_xvldrepl_d (void *_1)
{
  return __builtin_lasx_xvldrepl_d (_1, 8);
}
int
__lasx_xvpickve2gr_w (v8i32 _1)
{
  return __builtin_lasx_xvpickve2gr_w (_1, 1);
}
unsigned int
__lasx_xvpickve2gr_wu (v8i32 _1)
{
  return __builtin_lasx_xvpickve2gr_wu (_1, 1);
}
long
__lasx_xvpickve2gr_d (v4i64 _1)
{
  return __builtin_lasx_xvpickve2gr_d (_1, 1);
}
unsigned long int
__lasx_xvpickve2gr_du (v4i64 _1)
{
  return __builtin_lasx_xvpickve2gr_du (_1, 1);
}
v4i64
__lasx_xvaddwev_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvaddwev_q_d (_1, _2);
}
v4i64
__lasx_xvaddwev_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvaddwev_d_w (_1, _2);
}
v8i32
__lasx_xvaddwev_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvaddwev_w_h (_1, _2);
}
v16i16
__lasx_xvaddwev_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvaddwev_h_b (_1, _2);
}
v4i64
__lasx_xvaddwev_q_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvaddwev_q_du (_1, _2);
}
v4i64
__lasx_xvaddwev_d_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvaddwev_d_wu (_1, _2);
}
v8i32
__lasx_xvaddwev_w_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvaddwev_w_hu (_1, _2);
}
v16i16
__lasx_xvaddwev_h_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvaddwev_h_bu (_1, _2);
}
v4i64
__lasx_xvsubwev_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsubwev_q_d (_1, _2);
}
v4i64
__lasx_xvsubwev_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsubwev_d_w (_1, _2);
}
v8i32
__lasx_xvsubwev_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsubwev_w_h (_1, _2);
}
v16i16
__lasx_xvsubwev_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsubwev_h_b (_1, _2);
}
v4i64
__lasx_xvsubwev_q_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvsubwev_q_du (_1, _2);
}
v4i64
__lasx_xvsubwev_d_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvsubwev_d_wu (_1, _2);
}
v8i32
__lasx_xvsubwev_w_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvsubwev_w_hu (_1, _2);
}
v16i16
__lasx_xvsubwev_h_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvsubwev_h_bu (_1, _2);
}
v4i64
__lasx_xvmulwev_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmulwev_q_d (_1, _2);
}
v4i64
__lasx_xvmulwev_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmulwev_d_w (_1, _2);
}
v8i32
__lasx_xvmulwev_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmulwev_w_h (_1, _2);
}
v16i16
__lasx_xvmulwev_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmulwev_h_b (_1, _2);
}
v4i64
__lasx_xvmulwev_q_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvmulwev_q_du (_1, _2);
}
v4i64
__lasx_xvmulwev_d_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvmulwev_d_wu (_1, _2);
}
v8i32
__lasx_xvmulwev_w_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvmulwev_w_hu (_1, _2);
}
v16i16
__lasx_xvmulwev_h_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvmulwev_h_bu (_1, _2);
}
v4i64
__lasx_xvaddwod_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvaddwod_q_d (_1, _2);
}
v4i64
__lasx_xvaddwod_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvaddwod_d_w (_1, _2);
}
v8i32
__lasx_xvaddwod_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvaddwod_w_h (_1, _2);
}
v16i16
__lasx_xvaddwod_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvaddwod_h_b (_1, _2);
}
v4i64
__lasx_xvaddwod_q_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvaddwod_q_du (_1, _2);
}
v4i64
__lasx_xvaddwod_d_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvaddwod_d_wu (_1, _2);
}
v8i32
__lasx_xvaddwod_w_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvaddwod_w_hu (_1, _2);
}
v16i16
__lasx_xvaddwod_h_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvaddwod_h_bu (_1, _2);
}
v4i64
__lasx_xvsubwod_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsubwod_q_d (_1, _2);
}
v4i64
__lasx_xvsubwod_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsubwod_d_w (_1, _2);
}
v8i32
__lasx_xvsubwod_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsubwod_w_h (_1, _2);
}
v16i16
__lasx_xvsubwod_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsubwod_h_b (_1, _2);
}
v4i64
__lasx_xvsubwod_q_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvsubwod_q_du (_1, _2);
}
v4i64
__lasx_xvsubwod_d_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvsubwod_d_wu (_1, _2);
}
v8i32
__lasx_xvsubwod_w_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvsubwod_w_hu (_1, _2);
}
v16i16
__lasx_xvsubwod_h_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvsubwod_h_bu (_1, _2);
}
v4i64
__lasx_xvmulwod_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmulwod_q_d (_1, _2);
}
v4i64
__lasx_xvmulwod_d_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmulwod_d_w (_1, _2);
}
v8i32
__lasx_xvmulwod_w_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmulwod_w_h (_1, _2);
}
v16i16
__lasx_xvmulwod_h_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmulwod_h_b (_1, _2);
}
v4i64
__lasx_xvmulwod_q_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvmulwod_q_du (_1, _2);
}
v4i64
__lasx_xvmulwod_d_wu (v8u32 _1, v8u32 _2)
{
  return __builtin_lasx_xvmulwod_d_wu (_1, _2);
}
v8i32
__lasx_xvmulwod_w_hu (v16u16 _1, v16u16 _2)
{
  return __builtin_lasx_xvmulwod_w_hu (_1, _2);
}
v16i16
__lasx_xvmulwod_h_bu (v32u8 _1, v32u8 _2)
{
  return __builtin_lasx_xvmulwod_h_bu (_1, _2);
}
v4i64
__lasx_xvaddwev_d_wu_w (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvaddwev_d_wu_w (_1, _2);
}
v8i32
__lasx_xvaddwev_w_hu_h (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvaddwev_w_hu_h (_1, _2);
}
v16i16
__lasx_xvaddwev_h_bu_b (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvaddwev_h_bu_b (_1, _2);
}
v4i64
__lasx_xvmulwev_d_wu_w (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmulwev_d_wu_w (_1, _2);
}
v8i32
__lasx_xvmulwev_w_hu_h (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmulwev_w_hu_h (_1, _2);
}
v16i16
__lasx_xvmulwev_h_bu_b (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmulwev_h_bu_b (_1, _2);
}
v4i64
__lasx_xvaddwod_d_wu_w (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvaddwod_d_wu_w (_1, _2);
}
v8i32
__lasx_xvaddwod_w_hu_h (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvaddwod_w_hu_h (_1, _2);
}
v16i16
__lasx_xvaddwod_h_bu_b (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvaddwod_h_bu_b (_1, _2);
}
v4i64
__lasx_xvmulwod_d_wu_w (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvmulwod_d_wu_w (_1, _2);
}
v8i32
__lasx_xvmulwod_w_hu_h (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvmulwod_w_hu_h (_1, _2);
}
v16i16
__lasx_xvmulwod_h_bu_b (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvmulwod_h_bu_b (_1, _2);
}
v4i64
__lasx_xvhaddw_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvhaddw_q_d (_1, _2);
}
v4u64
__lasx_xvhaddw_qu_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvhaddw_qu_du (_1, _2);
}
v4i64
__lasx_xvhsubw_q_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvhsubw_q_d (_1, _2);
}
v4u64
__lasx_xvhsubw_qu_du (v4u64 _1, v4u64 _2)
{
  return __builtin_lasx_xvhsubw_qu_du (_1, _2);
}
v4i64
__lasx_xvmaddwev_q_d (v4i64 _1, v4i64 _2, v4i64 _3)
{
  return __builtin_lasx_xvmaddwev_q_d (_1, _2, _3);
}
v4i64
__lasx_xvmaddwev_d_w (v4i64 _1, v8i32 _2, v8i32 _3)
{
  return __builtin_lasx_xvmaddwev_d_w (_1, _2, _3);
}
v8i32
__lasx_xvmaddwev_w_h (v8i32 _1, v16i16 _2, v16i16 _3)
{
  return __builtin_lasx_xvmaddwev_w_h (_1, _2, _3);
}
v16i16
__lasx_xvmaddwev_h_b (v16i16 _1, v32i8 _2, v32i8 _3)
{
  return __builtin_lasx_xvmaddwev_h_b (_1, _2, _3);
}
v4u64
__lasx_xvmaddwev_q_du (v4u64 _1, v4u64 _2, v4u64 _3)
{
  return __builtin_lasx_xvmaddwev_q_du (_1, _2, _3);
}
v4u64
__lasx_xvmaddwev_d_wu (v4u64 _1, v8u32 _2, v8u32 _3)
{
  return __builtin_lasx_xvmaddwev_d_wu (_1, _2, _3);
}
v8u32
__lasx_xvmaddwev_w_hu (v8u32 _1, v16u16 _2, v16u16 _3)
{
  return __builtin_lasx_xvmaddwev_w_hu (_1, _2, _3);
}
v16u16
__lasx_xvmaddwev_h_bu (v16u16 _1, v32u8 _2, v32u8 _3)
{
  return __builtin_lasx_xvmaddwev_h_bu (_1, _2, _3);
}
v4i64
__lasx_xvmaddwod_q_d (v4i64 _1, v4i64 _2, v4i64 _3)
{
  return __builtin_lasx_xvmaddwod_q_d (_1, _2, _3);
}
v4i64
__lasx_xvmaddwod_d_w (v4i64 _1, v8i32 _2, v8i32 _3)
{
  return __builtin_lasx_xvmaddwod_d_w (_1, _2, _3);
}
v8i32
__lasx_xvmaddwod_w_h (v8i32 _1, v16i16 _2, v16i16 _3)
{
  return __builtin_lasx_xvmaddwod_w_h (_1, _2, _3);
}
v16i16
__lasx_xvmaddwod_h_b (v16i16 _1, v32i8 _2, v32i8 _3)
{
  return __builtin_lasx_xvmaddwod_h_b (_1, _2, _3);
}
v4u64
__lasx_xvmaddwod_q_du (v4u64 _1, v4u64 _2, v4u64 _3)
{
  return __builtin_lasx_xvmaddwod_q_du (_1, _2, _3);
}
v4u64
__lasx_xvmaddwod_d_wu (v4u64 _1, v8u32 _2, v8u32 _3)
{
  return __builtin_lasx_xvmaddwod_d_wu (_1, _2, _3);
}
v8u32
__lasx_xvmaddwod_w_hu (v8u32 _1, v16u16 _2, v16u16 _3)
{
  return __builtin_lasx_xvmaddwod_w_hu (_1, _2, _3);
}
v16u16
__lasx_xvmaddwod_h_bu (v16u16 _1, v32u8 _2, v32u8 _3)
{
  return __builtin_lasx_xvmaddwod_h_bu (_1, _2, _3);
}
v4i64
__lasx_xvmaddwev_q_du_d (v4i64 _1, v4u64 _2, v4i64 _3)
{
  return __builtin_lasx_xvmaddwev_q_du_d (_1, _2, _3);
}
v4i64
__lasx_xvmaddwev_d_wu_w (v4i64 _1, v8u32 _2, v8i32 _3)
{
  return __builtin_lasx_xvmaddwev_d_wu_w (_1, _2, _3);
}
v8i32
__lasx_xvmaddwev_w_hu_h (v8i32 _1, v16u16 _2, v16i16 _3)
{
  return __builtin_lasx_xvmaddwev_w_hu_h (_1, _2, _3);
}
v16i16
__lasx_xvmaddwev_h_bu_b (v16i16 _1, v32u8 _2, v32i8 _3)
{
  return __builtin_lasx_xvmaddwev_h_bu_b (_1, _2, _3);
}
v4i64
__lasx_xvmaddwod_q_du_d (v4i64 _1, v4u64 _2, v4i64 _3)
{
  return __builtin_lasx_xvmaddwod_q_du_d (_1, _2, _3);
}
v4i64
__lasx_xvmaddwod_d_wu_w (v4i64 _1, v8u32 _2, v8i32 _3)
{
  return __builtin_lasx_xvmaddwod_d_wu_w (_1, _2, _3);
}
v8i32
__lasx_xvmaddwod_w_hu_h (v8i32 _1, v16u16 _2, v16i16 _3)
{
  return __builtin_lasx_xvmaddwod_w_hu_h (_1, _2, _3);
}
v16i16
__lasx_xvmaddwod_h_bu_b (v16i16 _1, v32u8 _2, v32i8 _3)
{
  return __builtin_lasx_xvmaddwod_h_bu_b (_1, _2, _3);
}
v32i8
__lasx_xvrotr_b (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvrotr_b (_1, _2);
}
v16i16
__lasx_xvrotr_h (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvrotr_h (_1, _2);
}
v8i32
__lasx_xvrotr_w (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvrotr_w (_1, _2);
}
v4i64
__lasx_xvrotr_d (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvrotr_d (_1, _2);
}
v4i64
__lasx_xvadd_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvadd_q (_1, _2);
}
v4i64
__lasx_xvsub_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsub_q (_1, _2);
}
v4i64
__lasx_xvaddwev_q_du_d (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvaddwev_q_du_d (_1, _2);
}
v4i64
__lasx_xvaddwod_q_du_d (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvaddwod_q_du_d (_1, _2);
}
v4i64
__lasx_xvmulwev_q_du_d (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmulwev_q_du_d (_1, _2);
}
v4i64
__lasx_xvmulwod_q_du_d (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvmulwod_q_du_d (_1, _2);
}
v32i8
__lasx_xvmskgez_b (v32i8 _1)
{
  return __builtin_lasx_xvmskgez_b (_1);
}
v32i8
__lasx_xvmsknz_b (v32i8 _1)
{
  return __builtin_lasx_xvmsknz_b (_1);
}
v16i16
__lasx_xvexth_h_b (v32i8 _1)
{
  return __builtin_lasx_xvexth_h_b (_1);
}
v8i32
__lasx_xvexth_w_h (v16i16 _1)
{
  return __builtin_lasx_xvexth_w_h (_1);
}
v4i64
__lasx_xvexth_d_w (v8i32 _1)
{
  return __builtin_lasx_xvexth_d_w (_1);
}
v4i64
__lasx_xvexth_q_d (v4i64 _1)
{
  return __builtin_lasx_xvexth_q_d (_1);
}
v16u16
__lasx_xvexth_hu_bu (v32u8 _1)
{
  return __builtin_lasx_xvexth_hu_bu (_1);
}
v8u32
__lasx_xvexth_wu_hu (v16u16 _1)
{
  return __builtin_lasx_xvexth_wu_hu (_1);
}
v4u64
__lasx_xvexth_du_wu (v8u32 _1)
{
  return __builtin_lasx_xvexth_du_wu (_1);
}
v4u64
__lasx_xvexth_qu_du (v4u64 _1)
{
  return __builtin_lasx_xvexth_qu_du (_1);
}
v32i8
__lasx_xvrotri_b (v32i8 _1)
{
  return __builtin_lasx_xvrotri_b (_1, 1);
}
v16i16
__lasx_xvrotri_h (v16i16 _1)
{
  return __builtin_lasx_xvrotri_h (_1, 1);
}
v8i32
__lasx_xvrotri_w (v8i32 _1)
{
  return __builtin_lasx_xvrotri_w (_1, 1);
}
v4i64
__lasx_xvrotri_d (v4i64 _1)
{
  return __builtin_lasx_xvrotri_d (_1, 1);
}
v4i64
__lasx_xvextl_q_d (v4i64 _1)
{
  return __builtin_lasx_xvextl_q_d (_1);
}
v32i8
__lasx_xvsrlni_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrlni_b_h (_1, _2, 1);
}
v16i16
__lasx_xvsrlni_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrlni_h_w (_1, _2, 1);
}
v8i32
__lasx_xvsrlni_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrlni_w_d (_1, _2, 1);
}
v4i64
__lasx_xvsrlni_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrlni_d_q (_1, _2, 1);
}
v32i8
__lasx_xvsrlrni_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrlrni_b_h (_1, _2, 1);
}
v16i16
__lasx_xvsrlrni_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrlrni_h_w (_1, _2, 1);
}
v8i32
__lasx_xvsrlrni_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrlrni_w_d (_1, _2, 1);
}
v4i64
__lasx_xvsrlrni_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrlrni_d_q (_1, _2, 1);
}
v32i8
__lasx_xvssrlni_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrlni_b_h (_1, _2, 1);
}
v16i16
__lasx_xvssrlni_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrlni_h_w (_1, _2, 1);
}
v8i32
__lasx_xvssrlni_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrlni_w_d (_1, _2, 1);
}
v4i64
__lasx_xvssrlni_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrlni_d_q (_1, _2, 1);
}
v32u8
__lasx_xvssrlni_bu_h (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrlni_bu_h (_1, _2, 1);
}
v16u16
__lasx_xvssrlni_hu_w (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrlni_hu_w (_1, _2, 1);
}
v8u32
__lasx_xvssrlni_wu_d (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrlni_wu_d (_1, _2, 1);
}
v4u64
__lasx_xvssrlni_du_q (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrlni_du_q (_1, _2, 1);
}
v32i8
__lasx_xvssrlrni_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrlrni_b_h (_1, _2, 1);
}
v16i16
__lasx_xvssrlrni_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrlrni_h_w (_1, _2, 1);
}
v8i32
__lasx_xvssrlrni_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrlrni_w_d (_1, _2, 1);
}
v4i64
__lasx_xvssrlrni_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrlrni_d_q (_1, _2, 1);
}
v32u8
__lasx_xvssrlrni_bu_h (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrlrni_bu_h (_1, _2, 1);
}
v16u16
__lasx_xvssrlrni_hu_w (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrlrni_hu_w (_1, _2, 1);
}
v8u32
__lasx_xvssrlrni_wu_d (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrlrni_wu_d (_1, _2, 1);
}
v4u64
__lasx_xvssrlrni_du_q (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrlrni_du_q (_1, _2, 1);
}
v32i8
__lasx_xvsrani_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrani_b_h (_1, _2, 1);
}
v16i16
__lasx_xvsrani_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrani_h_w (_1, _2, 1);
}
v8i32
__lasx_xvsrani_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrani_w_d (_1, _2, 1);
}
v4i64
__lasx_xvsrani_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrani_d_q (_1, _2, 1);
}
v32i8
__lasx_xvsrarni_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvsrarni_b_h (_1, _2, 1);
}
v16i16
__lasx_xvsrarni_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvsrarni_h_w (_1, _2, 1);
}
v8i32
__lasx_xvsrarni_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvsrarni_w_d (_1, _2, 1);
}
v4i64
__lasx_xvsrarni_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvsrarni_d_q (_1, _2, 1);
}
v32i8
__lasx_xvssrani_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrani_b_h (_1, _2, 1);
}
v16i16
__lasx_xvssrani_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrani_h_w (_1, _2, 1);
}
v8i32
__lasx_xvssrani_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrani_w_d (_1, _2, 1);
}
v4i64
__lasx_xvssrani_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrani_d_q (_1, _2, 1);
}
v32u8
__lasx_xvssrani_bu_h (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrani_bu_h (_1, _2, 1);
}
v16u16
__lasx_xvssrani_hu_w (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrani_hu_w (_1, _2, 1);
}
v8u32
__lasx_xvssrani_wu_d (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrani_wu_d (_1, _2, 1);
}
v4u64
__lasx_xvssrani_du_q (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrani_du_q (_1, _2, 1);
}
v32i8
__lasx_xvssrarni_b_h (v32i8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrarni_b_h (_1, _2, 1);
}
v16i16
__lasx_xvssrarni_h_w (v16i16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrarni_h_w (_1, _2, 1);
}
v8i32
__lasx_xvssrarni_w_d (v8i32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrarni_w_d (_1, _2, 1);
}
v4i64
__lasx_xvssrarni_d_q (v4i64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrarni_d_q (_1, _2, 1);
}
v32u8
__lasx_xvssrarni_bu_h (v32u8 _1, v32i8 _2)
{
  return __builtin_lasx_xvssrarni_bu_h (_1, _2, 1);
}
v16u16
__lasx_xvssrarni_hu_w (v16u16 _1, v16i16 _2)
{
  return __builtin_lasx_xvssrarni_hu_w (_1, _2, 1);
}
v8u32
__lasx_xvssrarni_wu_d (v8u32 _1, v8i32 _2)
{
  return __builtin_lasx_xvssrarni_wu_d (_1, _2, 1);
}
v4u64
__lasx_xvssrarni_du_q (v4u64 _1, v4i64 _2)
{
  return __builtin_lasx_xvssrarni_du_q (_1, _2, 1);
}
int
__lasx_xbnz_b (v32u8 _1)
{
  return __builtin_lasx_xbnz_b (_1);
}
int
__lasx_xbnz_d (v4u64 _1)
{
  return __builtin_lasx_xbnz_d (_1);
}
int
__lasx_xbnz_h (v16u16 _1)
{
  return __builtin_lasx_xbnz_h (_1);
}
int
__lasx_xbnz_v (v32u8 _1)
{
  return __builtin_lasx_xbnz_v (_1);
}
int
__lasx_xbnz_w (v8u32 _1)
{
  return __builtin_lasx_xbnz_w (_1);
}
int
__lasx_xbz_b (v32u8 _1)
{
  return __builtin_lasx_xbz_b (_1);
}
int
__lasx_xbz_d (v4u64 _1)
{
  return __builtin_lasx_xbz_d (_1);
}
int
__lasx_xbz_h (v16u16 _1)
{
  return __builtin_lasx_xbz_h (_1);
}
int
__lasx_xbz_v (v32u8 _1)
{
  return __builtin_lasx_xbz_v (_1);
}
int
__lasx_xbz_w (v8u32 _1)
{
  return __builtin_lasx_xbz_w (_1);
}
v4i64
__lasx_xvfcmp_caf_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_caf_d (_1, _2);
}
v8i32
__lasx_xvfcmp_caf_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_caf_s (_1, _2);
}
v4i64
__lasx_xvfcmp_ceq_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_ceq_d (_1, _2);
}
v8i32
__lasx_xvfcmp_ceq_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_ceq_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cle_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cle_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cle_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cle_s (_1, _2);
}
v4i64
__lasx_xvfcmp_clt_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_clt_d (_1, _2);
}
v8i32
__lasx_xvfcmp_clt_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_clt_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cne_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cne_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cne_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cne_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cor_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cor_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cor_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cor_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cueq_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cueq_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cueq_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cueq_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cule_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cule_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cule_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cule_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cult_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cult_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cult_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cult_s (_1, _2);
}
v4i64
__lasx_xvfcmp_cun_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cun_d (_1, _2);
}
v4i64
__lasx_xvfcmp_cune_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_cune_d (_1, _2);
}
v8i32
__lasx_xvfcmp_cune_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cune_s (_1, _2);
}
v8i32
__lasx_xvfcmp_cun_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_cun_s (_1, _2);
}
v4i64
__lasx_xvfcmp_saf_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_saf_d (_1, _2);
}
v8i32
__lasx_xvfcmp_saf_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_saf_s (_1, _2);
}
v4i64
__lasx_xvfcmp_seq_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_seq_d (_1, _2);
}
v8i32
__lasx_xvfcmp_seq_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_seq_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sle_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sle_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sle_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sle_s (_1, _2);
}
v4i64
__lasx_xvfcmp_slt_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_slt_d (_1, _2);
}
v8i32
__lasx_xvfcmp_slt_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_slt_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sne_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sne_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sne_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sne_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sor_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sor_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sor_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sor_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sueq_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sueq_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sueq_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sueq_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sule_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sule_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sule_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sule_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sult_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sult_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sult_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sult_s (_1, _2);
}
v4i64
__lasx_xvfcmp_sun_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sun_d (_1, _2);
}
v4i64
__lasx_xvfcmp_sune_d (v4f64 _1, v4f64 _2)
{
  return __builtin_lasx_xvfcmp_sune_d (_1, _2);
}
v8i32
__lasx_xvfcmp_sune_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sune_s (_1, _2);
}
v8i32
__lasx_xvfcmp_sun_s (v8f32 _1, v8f32 _2)
{
  return __builtin_lasx_xvfcmp_sun_s (_1, _2);
}
v4f64
__lasx_xvpickve_d_f (v4f64 _1)
{
  return __builtin_lasx_xvpickve_d_f (_1, 1);
}
v8f32
__lasx_xvpickve_w_f (v8f32 _1)
{
  return __builtin_lasx_xvpickve_w_f (_1, 1);
}
v32i8
__lasx_xvrepli_b ()
{
  return __builtin_lasx_xvrepli_b (1);
}
v4i64
__lasx_xvrepli_d ()
{
  return __builtin_lasx_xvrepli_d (1);
}
v16i16
__lasx_xvrepli_h ()
{
  return __builtin_lasx_xvrepli_h (1);
}
v8i32
__lasx_xvrepli_w ()
{
  return __builtin_lasx_xvrepli_w (1);
}
