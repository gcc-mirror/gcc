/* Test builtins for LOONGARCH LSX ASE instructions */
/* { dg-do compile } */
/* { dg-options "-mlsx" } */
/* { dg-final { scan-assembler-times "lsx_vsll_b:.*vsll\\.b.*lsx_vsll_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsll_h:.*vsll\\.h.*lsx_vsll_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsll_w:.*vsll\\.w.*lsx_vsll_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsll_d:.*vsll\\.d.*lsx_vsll_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslli_b:.*vslli\\.b.*lsx_vslli_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslli_h:.*vslli\\.h.*lsx_vslli_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslli_w:.*vslli\\.w.*lsx_vslli_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslli_d:.*vslli\\.d.*lsx_vslli_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsra_b:.*vsra\\.b.*lsx_vsra_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsra_h:.*vsra\\.h.*lsx_vsra_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsra_w:.*vsra\\.w.*lsx_vsra_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsra_d:.*vsra\\.d.*lsx_vsra_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrai_b:.*vsrai\\.b.*lsx_vsrai_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrai_h:.*vsrai\\.h.*lsx_vsrai_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrai_w:.*vsrai\\.w.*lsx_vsrai_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrai_d:.*vsrai\\.d.*lsx_vsrai_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrar_b:.*vsrar\\.b.*lsx_vsrar_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrar_h:.*vsrar\\.h.*lsx_vsrar_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrar_w:.*vsrar\\.w.*lsx_vsrar_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrar_d:.*vsrar\\.d.*lsx_vsrar_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrari_b:.*vsrari\\.b.*lsx_vsrari_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrari_h:.*vsrari\\.h.*lsx_vsrari_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrari_w:.*vsrari\\.w.*lsx_vsrari_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrari_d:.*vsrari\\.d.*lsx_vsrari_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrl_b:.*vsrl\\.b.*lsx_vsrl_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrl_h:.*vsrl\\.h.*lsx_vsrl_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrl_w:.*vsrl\\.w.*lsx_vsrl_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrl_d:.*vsrl\\.d.*lsx_vsrl_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrli_b:.*vsrli\\.b.*lsx_vsrli_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrli_h:.*vsrli\\.h.*lsx_vsrli_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrli_w:.*vsrli\\.w.*lsx_vsrli_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrli_d:.*vsrli\\.d.*lsx_vsrli_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlr_b:.*vsrlr\\.b.*lsx_vsrlr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlr_h:.*vsrlr\\.h.*lsx_vsrlr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlr_w:.*vsrlr\\.w.*lsx_vsrlr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlr_d:.*vsrlr\\.d.*lsx_vsrlr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlri_b:.*vsrlri\\.b.*lsx_vsrlri_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlri_h:.*vsrlri\\.h.*lsx_vsrlri_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlri_w:.*vsrlri\\.w.*lsx_vsrlri_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlri_d:.*vsrlri\\.d.*lsx_vsrlri_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclr_b:.*vbitclr\\.b.*lsx_vbitclr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclr_h:.*vbitclr\\.h.*lsx_vbitclr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclr_w:.*vbitclr\\.w.*lsx_vbitclr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclr_d:.*vbitclr\\.d.*lsx_vbitclr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclri_b:.*vbitclri\\.b.*lsx_vbitclri_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclri_h:.*vbitclri\\.h.*lsx_vbitclri_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclri_w:.*vbitclri\\.w.*lsx_vbitclri_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitclri_d:.*vbitclri\\.d.*lsx_vbitclri_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitset_b:.*vbitset\\.b.*lsx_vbitset_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitset_h:.*vbitset\\.h.*lsx_vbitset_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitset_w:.*vbitset\\.w.*lsx_vbitset_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitset_d:.*vbitset\\.d.*lsx_vbitset_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitseti_b:.*vbitseti\\.b.*lsx_vbitseti_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitseti_h:.*vbitseti\\.h.*lsx_vbitseti_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitseti_w:.*vbitseti\\.w.*lsx_vbitseti_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitseti_d:.*vbitseti\\.d.*lsx_vbitseti_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrev_b:.*vbitrev\\.b.*lsx_vbitrev_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrev_h:.*vbitrev\\.h.*lsx_vbitrev_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrev_w:.*vbitrev\\.w.*lsx_vbitrev_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrev_d:.*vbitrev\\.d.*lsx_vbitrev_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrevi_b:.*vbitrevi\\.b.*lsx_vbitrevi_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrevi_h:.*vbitrevi\\.h.*lsx_vbitrevi_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrevi_w:.*vbitrevi\\.w.*lsx_vbitrevi_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitrevi_d:.*vbitrevi\\.d.*lsx_vbitrevi_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadd_b:.*vadd\\.b.*lsx_vadd_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadd_h:.*vadd\\.h.*lsx_vadd_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadd_w:.*vadd\\.w.*lsx_vadd_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadd_d:.*vadd\\.d.*lsx_vadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddi_bu:.*vaddi\\.bu.*lsx_vaddi_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddi_hu:.*vaddi\\.hu.*lsx_vaddi_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddi_wu:.*vaddi\\.wu.*lsx_vaddi_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddi_du:.*vaddi\\.du.*lsx_vaddi_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsub_b:.*vsub\\.b.*lsx_vsub_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsub_h:.*vsub\\.h.*lsx_vsub_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsub_w:.*vsub\\.w.*lsx_vsub_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsub_d:.*vsub\\.d.*lsx_vsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubi_bu:.*vsubi\\.bu.*lsx_vsubi_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubi_hu:.*vsubi\\.hu.*lsx_vsubi_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubi_wu:.*vsubi\\.wu.*lsx_vsubi_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubi_du:.*vsubi\\.du.*lsx_vsubi_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_b:.*vmax\\.b.*lsx_vmax_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_h:.*vmax\\.h.*lsx_vmax_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_w:.*vmax\\.w.*lsx_vmax_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_d:.*vmax\\.d.*lsx_vmax_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_b:.*vmaxi\\.b.*lsx_vmaxi_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_h:.*vmaxi\\.h.*lsx_vmaxi_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_w:.*vmaxi\\.w.*lsx_vmaxi_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_d:.*vmaxi\\.d.*lsx_vmaxi_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_bu:.*vmax\\.bu.*lsx_vmax_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_hu:.*vmax\\.hu.*lsx_vmax_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_wu:.*vmax\\.wu.*lsx_vmax_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmax_du:.*vmax\\.du.*lsx_vmax_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_bu:.*vmaxi\\.bu.*lsx_vmaxi_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_hu:.*vmaxi\\.hu.*lsx_vmaxi_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_wu:.*vmaxi\\.wu.*lsx_vmaxi_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaxi_du:.*vmaxi\\.du.*lsx_vmaxi_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_b:.*vmin\\.b.*lsx_vmin_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_h:.*vmin\\.h.*lsx_vmin_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_w:.*vmin\\.w.*lsx_vmin_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_d:.*vmin\\.d.*lsx_vmin_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_b:.*vmini\\.b.*lsx_vmini_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_h:.*vmini\\.h.*lsx_vmini_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_w:.*vmini\\.w.*lsx_vmini_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_d:.*vmini\\.d.*lsx_vmini_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_bu:.*vmin\\.bu.*lsx_vmin_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_hu:.*vmin\\.hu.*lsx_vmin_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_wu:.*vmin\\.wu.*lsx_vmin_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmin_du:.*vmin\\.du.*lsx_vmin_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_bu:.*vmini\\.bu.*lsx_vmini_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_hu:.*vmini\\.hu.*lsx_vmini_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_wu:.*vmini\\.wu.*lsx_vmini_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmini_du:.*vmini\\.du.*lsx_vmini_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseq_b:.*vseq\\.b.*lsx_vseq_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseq_h:.*vseq\\.h.*lsx_vseq_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseq_w:.*vseq\\.w.*lsx_vseq_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseq_d:.*vseq\\.d.*lsx_vseq_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseqi_b:.*vseqi\\.b.*lsx_vseqi_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseqi_h:.*vseqi\\.h.*lsx_vseqi_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseqi_w:.*vseqi\\.w.*lsx_vseqi_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vseqi_d:.*vseqi\\.d.*lsx_vseqi_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_b:.*vslti\\.b.*lsx_vslti_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_b:.*vslt\\.b.*lsx_vslt_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_h:.*vslt\\.h.*lsx_vslt_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_w:.*vslt\\.w.*lsx_vslt_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_d:.*vslt\\.d.*lsx_vslt_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_h:.*vslti\\.h.*lsx_vslti_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_w:.*vslti\\.w.*lsx_vslti_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_d:.*vslti\\.d.*lsx_vslti_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_bu:.*vslt\\.bu.*lsx_vslt_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_hu:.*vslt\\.hu.*lsx_vslt_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_wu:.*vslt\\.wu.*lsx_vslt_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslt_du:.*vslt\\.du.*lsx_vslt_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_bu:.*vslti\\.bu.*lsx_vslti_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_hu:.*vslti\\.hu.*lsx_vslti_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_wu:.*vslti\\.wu.*lsx_vslti_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslti_du:.*vslti\\.du.*lsx_vslti_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_b:.*vsle\\.b.*lsx_vsle_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_h:.*vsle\\.h.*lsx_vsle_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_w:.*vsle\\.w.*lsx_vsle_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_d:.*vsle\\.d.*lsx_vsle_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_b:.*vslei\\.b.*lsx_vslei_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_h:.*vslei\\.h.*lsx_vslei_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_w:.*vslei\\.w.*lsx_vslei_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_d:.*vslei\\.d.*lsx_vslei_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_bu:.*vsle\\.bu.*lsx_vsle_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_hu:.*vsle\\.hu.*lsx_vsle_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_wu:.*vsle\\.wu.*lsx_vsle_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsle_du:.*vsle\\.du.*lsx_vsle_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_bu:.*vslei\\.bu.*lsx_vslei_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_hu:.*vslei\\.hu.*lsx_vslei_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_wu:.*vslei\\.wu.*lsx_vslei_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vslei_du:.*vslei\\.du.*lsx_vslei_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_b:.*vsat\\.b.*lsx_vsat_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_h:.*vsat\\.h.*lsx_vsat_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_w:.*vsat\\.w.*lsx_vsat_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_d:.*vsat\\.d.*lsx_vsat_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_bu:.*vsat\\.bu.*lsx_vsat_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_hu:.*vsat\\.hu.*lsx_vsat_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_wu:.*vsat\\.wu.*lsx_vsat_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsat_du:.*vsat\\.du.*lsx_vsat_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadda_b:.*vadda\\.b.*lsx_vadda_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadda_h:.*vadda\\.h.*lsx_vadda_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadda_w:.*vadda\\.w.*lsx_vadda_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadda_d:.*vadda\\.d.*lsx_vadda_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_b:.*vsadd\\.b.*lsx_vsadd_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_h:.*vsadd\\.h.*lsx_vsadd_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_w:.*vsadd\\.w.*lsx_vsadd_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_d:.*vsadd\\.d.*lsx_vsadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_bu:.*vsadd\\.bu.*lsx_vsadd_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_hu:.*vsadd\\.hu.*lsx_vsadd_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_wu:.*vsadd\\.wu.*lsx_vsadd_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsadd_du:.*vsadd\\.du.*lsx_vsadd_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_b:.*vavg\\.b.*lsx_vavg_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_h:.*vavg\\.h.*lsx_vavg_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_w:.*vavg\\.w.*lsx_vavg_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_d:.*vavg\\.d.*lsx_vavg_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_bu:.*vavg\\.bu.*lsx_vavg_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_hu:.*vavg\\.hu.*lsx_vavg_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_wu:.*vavg\\.wu.*lsx_vavg_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavg_du:.*vavg\\.du.*lsx_vavg_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_b:.*vavgr\\.b.*lsx_vavgr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_h:.*vavgr\\.h.*lsx_vavgr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_w:.*vavgr\\.w.*lsx_vavgr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_d:.*vavgr\\.d.*lsx_vavgr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_bu:.*vavgr\\.bu.*lsx_vavgr_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_hu:.*vavgr\\.hu.*lsx_vavgr_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_wu:.*vavgr\\.wu.*lsx_vavgr_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vavgr_du:.*vavgr\\.du.*lsx_vavgr_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_b:.*vssub\\.b.*lsx_vssub_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_h:.*vssub\\.h.*lsx_vssub_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_w:.*vssub\\.w.*lsx_vssub_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_d:.*vssub\\.d.*lsx_vssub_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_bu:.*vssub\\.bu.*lsx_vssub_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_hu:.*vssub\\.hu.*lsx_vssub_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_wu:.*vssub\\.wu.*lsx_vssub_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssub_du:.*vssub\\.du.*lsx_vssub_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_b:.*vabsd\\.b.*lsx_vabsd_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_h:.*vabsd\\.h.*lsx_vabsd_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_w:.*vabsd\\.w.*lsx_vabsd_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_d:.*vabsd\\.d.*lsx_vabsd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_bu:.*vabsd\\.bu.*lsx_vabsd_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_hu:.*vabsd\\.hu.*lsx_vabsd_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_wu:.*vabsd\\.wu.*lsx_vabsd_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vabsd_du:.*vabsd\\.du.*lsx_vabsd_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmul_b:.*vmul\\.b.*lsx_vmul_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmul_h:.*vmul\\.h.*lsx_vmul_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmul_w:.*vmul\\.w.*lsx_vmul_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmul_d:.*vmul\\.d.*lsx_vmul_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmadd_b:.*vmadd\\.b.*lsx_vmadd_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmadd_h:.*vmadd\\.h.*lsx_vmadd_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmadd_w:.*vmadd\\.w.*lsx_vmadd_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmadd_d:.*vmadd\\.d.*lsx_vmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmsub_b:.*vmsub\\.b.*lsx_vmsub_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmsub_h:.*vmsub\\.h.*lsx_vmsub_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmsub_w:.*vmsub\\.w.*lsx_vmsub_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmsub_d:.*vmsub\\.d.*lsx_vmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_b:.*vdiv\\.b.*lsx_vdiv_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_h:.*vdiv\\.h.*lsx_vdiv_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_w:.*vdiv\\.w.*lsx_vdiv_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_d:.*vdiv\\.d.*lsx_vdiv_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_bu:.*vdiv\\.bu.*lsx_vdiv_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_hu:.*vdiv\\.hu.*lsx_vdiv_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_wu:.*vdiv\\.wu.*lsx_vdiv_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vdiv_du:.*vdiv\\.du.*lsx_vdiv_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_h_b:.*vhaddw\\.h\\.b.*lsx_vhaddw_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_w_h:.*vhaddw\\.w\\.h.*lsx_vhaddw_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_d_w:.*vhaddw\\.d\\.w.*lsx_vhaddw_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_hu_bu:.*vhaddw\\.hu\\.bu.*lsx_vhaddw_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_wu_hu:.*vhaddw\\.wu\\.hu.*lsx_vhaddw_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_du_wu:.*vhaddw\\.du\\.wu.*lsx_vhaddw_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_h_b:.*vhsubw\\.h\\.b.*lsx_vhsubw_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_w_h:.*vhsubw\\.w\\.h.*lsx_vhsubw_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_d_w:.*vhsubw\\.d\\.w.*lsx_vhsubw_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_hu_bu:.*vhsubw\\.hu\\.bu.*lsx_vhsubw_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_wu_hu:.*vhsubw\\.wu\\.hu.*lsx_vhsubw_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_du_wu:.*vhsubw\\.du\\.wu.*lsx_vhsubw_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_b:.*vmod\\.b.*lsx_vmod_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_h:.*vmod\\.h.*lsx_vmod_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_w:.*vmod\\.w.*lsx_vmod_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_d:.*vmod\\.d.*lsx_vmod_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_bu:.*vmod\\.bu.*lsx_vmod_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_hu:.*vmod\\.hu.*lsx_vmod_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_wu:.*vmod\\.wu.*lsx_vmod_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmod_du:.*vmod\\.du.*lsx_vmod_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplve_b:.*vreplve\\.b.*lsx_vreplve_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplve_h:.*vreplve\\.h.*lsx_vreplve_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplve_w:.*vreplve\\.w.*lsx_vreplve_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplve_d:.*vreplve\\.d.*lsx_vreplve_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplvei_b:.*vreplvei\\.b.*lsx_vreplvei_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplvei_h:.*vreplvei\\.h.*lsx_vreplvei_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplvei_w:.*vreplvei\\.w.*lsx_vreplvei_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplvei_d:.*vreplvei\\.d.*lsx_vreplvei_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickev_b:.*vpickev\\.b.*lsx_vpickev_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickev_h:.*vpickev\\.h.*lsx_vpickev_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickev_w:.*vpickev\\.w.*lsx_vpickev_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickev_d:.*vilvl\\.d.*lsx_vpickev_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickod_b:.*vpickod\\.b.*lsx_vpickod_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickod_h:.*vpickod\\.h.*lsx_vpickod_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickod_w:.*vpickod\\.w.*lsx_vpickod_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickod_d:.*vilvh\\.d.*lsx_vpickod_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvh_b:.*vilvh\\.b.*lsx_vilvh_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvh_h:.*vilvh\\.h.*lsx_vilvh_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvh_w:.*vilvh\\.w.*lsx_vilvh_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvh_d:.*vilvh\\.d.*lsx_vilvh_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvl_b:.*vilvl\\.b.*lsx_vilvl_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvl_h:.*vilvl\\.h.*lsx_vilvl_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvl_w:.*vilvl\\.w.*lsx_vilvl_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vilvl_d:.*vilvl\\.d.*lsx_vilvl_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackev_b:.*vpackev\\.b.*lsx_vpackev_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackev_h:.*vpackev\\.h.*lsx_vpackev_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackev_w:.*vpackev\\.w.*lsx_vpackev_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackev_d:.*vilvl\\.d.*lsx_vpackev_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackod_b:.*vpackod\\.b.*lsx_vpackod_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackod_h:.*vpackod\\.h.*lsx_vpackod_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackod_w:.*vpackod\\.w.*lsx_vpackod_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpackod_d:.*vilvh\\.d.*lsx_vpackod_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf_h:.*vshuf\\.h.*lsx_vshuf_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf_w:.*vshuf\\.w.*lsx_vshuf_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf_d:.*vshuf\\.d.*lsx_vshuf_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vand_v:.*vand\\.v.*lsx_vand_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vandi_b:.*vandi\\.b.*lsx_vandi_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vor_v:.*vor\\.v.*lsx_vor_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vori_b:.*vbitseti\\.b.*lsx_vori_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vnor_v:.*vnor\\.v.*lsx_vnor_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vnori_b:.*vnori\\.b.*lsx_vnori_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vxor_v:.*vxor\\.v.*lsx_vxor_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vxori_b:.*vbitrevi\\.b.*lsx_vxori_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitsel_v:.*vbitsel\\.v.*lsx_vbitsel_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbitseli_b:.*vbitseli\\.b.*lsx_vbitseli_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf4i_b:.*vshuf4i\\.b.*lsx_vshuf4i_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf4i_h:.*vshuf4i\\.h.*lsx_vshuf4i_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf4i_w:.*vshuf4i\\.w.*lsx_vshuf4i_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplgr2vr_b:.*vreplgr2vr\\.b.*lsx_vreplgr2vr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplgr2vr_h:.*vreplgr2vr\\.h.*lsx_vreplgr2vr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplgr2vr_w:.*vreplgr2vr\\.w.*lsx_vreplgr2vr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vreplgr2vr_d:.*vreplgr2vr\\.d.*lsx_vreplgr2vr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpcnt_b:.*vpcnt\\.b.*lsx_vpcnt_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpcnt_h:.*vpcnt\\.h.*lsx_vpcnt_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpcnt_w:.*vpcnt\\.w.*lsx_vpcnt_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpcnt_d:.*vpcnt\\.d.*lsx_vpcnt_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclo_b:.*vclo\\.b.*lsx_vclo_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclo_h:.*vclo\\.h.*lsx_vclo_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclo_w:.*vclo\\.w.*lsx_vclo_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclo_d:.*vclo\\.d.*lsx_vclo_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclz_b:.*vclz\\.b.*lsx_vclz_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclz_h:.*vclz\\.h.*lsx_vclz_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclz_w:.*vclz\\.w.*lsx_vclz_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vclz_d:.*vclz\\.d.*lsx_vclz_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_b:.*vpickve2gr\\.b.*lsx_vpickve2gr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_h:.*vpickve2gr\\.h.*lsx_vpickve2gr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_w:.*vpickve2gr\\.w.*lsx_vpickve2gr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_d:.*vpickve2gr\\.d.*lsx_vpickve2gr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_bu:.*vpickve2gr\\.bu.*lsx_vpickve2gr_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_hu:.*vpickve2gr\\.hu.*lsx_vpickve2gr_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_wu:.*vpickve2gr\\.wu.*lsx_vpickve2gr_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpickve2gr_du:.*vpickve2gr\\.du.*lsx_vpickve2gr_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vinsgr2vr_b:.*vinsgr2vr\\.b.*lsx_vinsgr2vr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vinsgr2vr_h:.*vinsgr2vr\\.h.*lsx_vinsgr2vr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vinsgr2vr_w:.*vinsgr2vr\\.w.*lsx_vinsgr2vr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vinsgr2vr_d:.*vinsgr2vr\\.d.*lsx_vinsgr2vr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfadd_s:.*vfadd\\.s.*lsx_vfadd_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfadd_d:.*vfadd\\.d.*lsx_vfadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfsub_s:.*vfsub\\.s.*lsx_vfsub_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfsub_d:.*vfsub\\.d.*lsx_vfsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmul_s:.*vfmul\\.s.*lsx_vfmul_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmul_d:.*vfmul\\.d.*lsx_vfmul_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfdiv_s:.*vfdiv\\.s.*lsx_vfdiv_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfdiv_d:.*vfdiv\\.d.*lsx_vfdiv_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcvt_h_s:.*vfcvt\\.h\\.s.*lsx_vfcvt_h_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcvt_s_d:.*vfcvt\\.s\\.d.*lsx_vfcvt_s_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmin_s:.*vfmin\\.s.*lsx_vfmin_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmin_d:.*vfmin\\.d.*lsx_vfmin_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmina_s:.*vfmina\\.s.*lsx_vfmina_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmina_d:.*vfmina\\.d.*lsx_vfmina_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmax_s:.*vfmax\\.s.*lsx_vfmax_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmax_d:.*vfmax\\.d.*lsx_vfmax_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmaxa_s:.*vfmaxa\\.s.*lsx_vfmaxa_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmaxa_d:.*vfmaxa\\.d.*lsx_vfmaxa_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfclass_s:.*vfclass\\.s.*lsx_vfclass_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfclass_d:.*vfclass\\.d.*lsx_vfclass_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfsqrt_s:.*vfsqrt\\.s.*lsx_vfsqrt_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfsqrt_d:.*vfsqrt\\.d.*lsx_vfsqrt_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrecip_s:.*vfrecip\\.s.*lsx_vfrecip_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrecip_d:.*vfrecip\\.d.*lsx_vfrecip_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrint_s:.*vfrint\\.s.*lsx_vfrint_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrint_d:.*vfrint\\.d.*lsx_vfrint_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrsqrt_s:.*vfrsqrt\\.s.*lsx_vfrsqrt_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrsqrt_d:.*vfrsqrt\\.d.*lsx_vfrsqrt_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vflogb_s:.*vflogb\\.s.*lsx_vflogb_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vflogb_d:.*vflogb\\.d.*lsx_vflogb_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcvth_s_h:.*vfcvth\\.s\\.h.*lsx_vfcvth_s_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcvth_d_s:.*vfcvth\\.d\\.s.*lsx_vfcvth_d_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcvtl_s_h:.*vfcvtl\\.s\\.h.*lsx_vfcvtl_s_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcvtl_d_s:.*vfcvtl\\.d\\.s.*lsx_vfcvtl_d_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftint_w_s:.*vftint\\.w\\.s.*lsx_vftint_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftint_l_d:.*vftint\\.l\\.d.*lsx_vftint_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftint_wu_s:.*vftint\\.wu\\.s.*lsx_vftint_wu_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftint_lu_d:.*vftint\\.lu\\.d.*lsx_vftint_lu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrz_w_s:.*vftintrz\\.w\\.s.*lsx_vftintrz_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrz_l_d:.*vftintrz\\.l\\.d.*lsx_vftintrz_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrz_wu_s:.*vftintrz\\.wu\\.s.*lsx_vftintrz_wu_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrz_lu_d:.*vftintrz\\.lu\\.d.*lsx_vftintrz_lu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffint_s_w:.*vffint\\.s\\.w.*lsx_vffint_s_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffint_d_l:.*vffint\\.d\\.l.*lsx_vffint_d_l" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffint_s_wu:.*vffint\\.s\\.wu.*lsx_vffint_s_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffint_d_lu:.*vffint\\.d\\.lu.*lsx_vffint_d_lu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vandn_v:.*vandn\\.v.*lsx_vandn_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vneg_b:.*vneg\\.b.*lsx_vneg_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vneg_h:.*vneg\\.h.*lsx_vneg_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vneg_w:.*vneg\\.w.*lsx_vneg_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vneg_d:.*vneg\\.d.*lsx_vneg_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_b:.*vmuh\\.b.*lsx_vmuh_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_h:.*vmuh\\.h.*lsx_vmuh_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_w:.*vmuh\\.w.*lsx_vmuh_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_d:.*vmuh\\.d.*lsx_vmuh_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_bu:.*vmuh\\.bu.*lsx_vmuh_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_hu:.*vmuh\\.hu.*lsx_vmuh_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_wu:.*vmuh\\.wu.*lsx_vmuh_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmuh_du:.*vmuh\\.du.*lsx_vmuh_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsllwil_h_b:.*vsllwil\\.h\\.b.*lsx_vsllwil_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsllwil_w_h:.*vsllwil\\.w\\.h.*lsx_vsllwil_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsllwil_d_w:.*vsllwil\\.d\\.w.*lsx_vsllwil_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsllwil_hu_bu:.*vsllwil\\.hu\\.bu.*lsx_vsllwil_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsllwil_wu_hu:.*vsllwil\\.wu\\.hu.*lsx_vsllwil_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsllwil_du_wu:.*vsllwil\\.du\\.wu.*lsx_vsllwil_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsran_b_h:.*vsran\\.b\\.h.*lsx_vsran_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsran_h_w:.*vsran\\.h\\.w.*lsx_vsran_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsran_w_d:.*vsran\\.w\\.d.*lsx_vsran_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssran_b_h:.*vssran\\.b\\.h.*lsx_vssran_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssran_h_w:.*vssran\\.h\\.w.*lsx_vssran_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssran_w_d:.*vssran\\.w\\.d.*lsx_vssran_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssran_bu_h:.*vssran\\.bu\\.h.*lsx_vssran_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssran_hu_w:.*vssran\\.hu\\.w.*lsx_vssran_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssran_wu_d:.*vssran\\.wu\\.d.*lsx_vssran_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarn_b_h:.*vsrarn\\.b\\.h.*lsx_vsrarn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarn_h_w:.*vsrarn\\.h\\.w.*lsx_vsrarn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarn_w_d:.*vsrarn\\.w\\.d.*lsx_vsrarn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarn_b_h:.*vssrarn\\.b\\.h.*lsx_vssrarn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarn_h_w:.*vssrarn\\.h\\.w.*lsx_vssrarn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarn_w_d:.*vssrarn\\.w\\.d.*lsx_vssrarn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarn_bu_h:.*vssrarn\\.bu\\.h.*lsx_vssrarn_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarn_hu_w:.*vssrarn\\.hu\\.w.*lsx_vssrarn_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarn_wu_d:.*vssrarn\\.wu\\.d.*lsx_vssrarn_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrln_b_h:.*vsrln\\.b\\.h.*lsx_vsrln_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrln_h_w:.*vsrln\\.h\\.w.*lsx_vsrln_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrln_w_d:.*vsrln\\.w\\.d.*lsx_vsrln_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrln_bu_h:.*vssrln\\.bu\\.h.*lsx_vssrln_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrln_hu_w:.*vssrln\\.hu\\.w.*lsx_vssrln_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrln_wu_d:.*vssrln\\.wu\\.d.*lsx_vssrln_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrn_b_h:.*vsrlrn\\.b\\.h.*lsx_vsrlrn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrn_h_w:.*vsrlrn\\.h\\.w.*lsx_vsrlrn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrn_w_d:.*vsrlrn\\.w\\.d.*lsx_vsrlrn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrn_bu_h:.*vssrlrn\\.bu\\.h.*lsx_vssrlrn_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrn_hu_w:.*vssrlrn\\.hu\\.w.*lsx_vssrlrn_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrn_wu_d:.*vssrlrn\\.wu\\.d.*lsx_vssrlrn_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrstpi_b:.*vfrstpi\\.b.*lsx_vfrstpi_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrstpi_h:.*vfrstpi\\.h.*lsx_vfrstpi_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrstp_b:.*vfrstp\\.b.*lsx_vfrstp_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrstp_h:.*vfrstp\\.h.*lsx_vfrstp_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf4i_d:.*vshuf4i\\.d.*lsx_vshuf4i_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbsrl_v:.*vbsrl\\.v.*lsx_vbsrl_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vbsll_v:.*vbsll\\.v.*lsx_vbsll_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vextrins_b:.*vextrins\\.b.*lsx_vextrins_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vextrins_h:.*vextrins\\.h.*lsx_vextrins_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vextrins_w:.*vextrins\\.w.*lsx_vextrins_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vextrins_d:.*vextrins\\.d.*lsx_vextrins_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmskltz_b:.*vmskltz\\.b.*lsx_vmskltz_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmskltz_h:.*vmskltz\\.h.*lsx_vmskltz_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmskltz_w:.*vmskltz\\.w.*lsx_vmskltz_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmskltz_d:.*vmskltz\\.d.*lsx_vmskltz_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsigncov_b:.*vsigncov\\.b.*lsx_vsigncov_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsigncov_h:.*vsigncov\\.h.*lsx_vsigncov_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsigncov_w:.*vsigncov\\.w.*lsx_vsigncov_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsigncov_d:.*vsigncov\\.d.*lsx_vsigncov_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmadd_s:.*vfmadd\\.s.*lsx_vfmadd_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmadd_d:.*vfmadd\\.d.*lsx_vfmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmsub_s:.*vfmsub\\.s.*lsx_vfmsub_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfmsub_d:.*vfmsub\\.d.*lsx_vfmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfnmadd_s:.*vfnmadd\\.s.*lsx_vfnmadd_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfnmadd_d:.*vfnmadd\\.d.*lsx_vfnmadd_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfnmsub_s:.*vfnmsub\\.s.*lsx_vfnmsub_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfnmsub_d:.*vfnmsub\\.d.*lsx_vfnmsub_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrne_w_s:.*vftintrne\\.w\\.s.*lsx_vftintrne_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrne_l_d:.*vftintrne\\.l\\.d.*lsx_vftintrne_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrp_w_s:.*vftintrp\\.w\\.s.*lsx_vftintrp_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrp_l_d:.*vftintrp\\.l\\.d.*lsx_vftintrp_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrm_w_s:.*vftintrm\\.w\\.s.*lsx_vftintrm_w_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrm_l_d:.*vftintrm\\.l\\.d.*lsx_vftintrm_l_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftint_w_d:.*vftint\\.w\\.d.*lsx_vftint_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffint_s_l:.*vffint\\.s\\.l.*lsx_vffint_s_l" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrz_w_d:.*vftintrz\\.w\\.d.*lsx_vftintrz_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrp_w_d:.*vftintrp\\.w\\.d.*lsx_vftintrp_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrm_w_d:.*vftintrm\\.w\\.d.*lsx_vftintrm_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrne_w_d:.*vftintrne\\.w\\.d.*lsx_vftintrne_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintl_l_s:.*vftintl\\.l\\.s.*lsx_vftintl_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftinth_l_s:.*vftinth\\.l\\.s.*lsx_vftinth_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffinth_d_w:.*vffinth\\.d\\.w.*lsx_vffinth_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vffintl_d_w:.*vffintl\\.d\\.w.*lsx_vffintl_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrzl_l_s:.*vftintrzl\\.l\\.s.*lsx_vftintrzl_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrzh_l_s:.*vftintrzh\\.l\\.s.*lsx_vftintrzh_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrpl_l_s:.*vftintrpl\\.l\\.s.*lsx_vftintrpl_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrph_l_s:.*vftintrph\\.l\\.s.*lsx_vftintrph_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrml_l_s:.*vftintrml\\.l\\.s.*lsx_vftintrml_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrmh_l_s:.*vftintrmh\\.l\\.s.*lsx_vftintrmh_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrnel_l_s:.*vftintrnel\\.l\\.s.*lsx_vftintrnel_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vftintrneh_l_s:.*vftintrneh\\.l\\.s.*lsx_vftintrneh_l_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrne_s:.*vfrintrne\\.s.*lsx_vfrintrne_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrne_d:.*vfrintrne\\.d.*lsx_vfrintrne_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrz_s:.*vfrintrz\\.s.*lsx_vfrintrz_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrz_d:.*vfrintrz\\.d.*lsx_vfrintrz_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrp_s:.*vfrintrp\\.s.*lsx_vfrintrp_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrp_d:.*vfrintrp\\.d.*lsx_vfrintrp_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrm_s:.*vfrintrm\\.s.*lsx_vfrintrm_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrintrm_d:.*vfrintrm\\.d.*lsx_vfrintrm_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vstelm_b:.*vstelm\\.b.*lsx_vstelm_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vstelm_h:.*vstelm\\.h.*lsx_vstelm_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vstelm_w:.*vstelm\\.w.*lsx_vstelm_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vstelm_d:.*vstelm\\.d.*lsx_vstelm_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_d_w:.*vaddwev\\.d\\.w.*lsx_vaddwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_w_h:.*vaddwev\\.w\\.h.*lsx_vaddwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_h_b:.*vaddwev\\.h\\.b.*lsx_vaddwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_d_w:.*vaddwod\\.d\\.w.*lsx_vaddwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_w_h:.*vaddwod\\.w\\.h.*lsx_vaddwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_h_b:.*vaddwod\\.h\\.b.*lsx_vaddwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_d_wu:.*vaddwev\\.d\\.wu.*lsx_vaddwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_w_hu:.*vaddwev\\.w\\.hu.*lsx_vaddwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_h_bu:.*vaddwev\\.h\\.bu.*lsx_vaddwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_d_wu:.*vaddwod\\.d\\.wu.*lsx_vaddwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_w_hu:.*vaddwod\\.w\\.hu.*lsx_vaddwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_h_bu:.*vaddwod\\.h\\.bu.*lsx_vaddwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_d_wu_w:.*vaddwev\\.d\\.wu\\.w.*lsx_vaddwev_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_w_hu_h:.*vaddwev\\.w\\.hu\\.h.*lsx_vaddwev_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_h_bu_b:.*vaddwev\\.h\\.bu\\.b.*lsx_vaddwev_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_d_wu_w:.*vaddwod\\.d\\.wu\\.w.*lsx_vaddwod_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_w_hu_h:.*vaddwod\\.w\\.hu\\.h.*lsx_vaddwod_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_h_bu_b:.*vaddwod\\.h\\.bu\\.b.*lsx_vaddwod_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_d_w:.*vsubwev\\.d\\.w.*lsx_vsubwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_w_h:.*vsubwev\\.w\\.h.*lsx_vsubwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_h_b:.*vsubwev\\.h\\.b.*lsx_vsubwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_d_w:.*vsubwod\\.d\\.w.*lsx_vsubwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_w_h:.*vsubwod\\.w\\.h.*lsx_vsubwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_h_b:.*vsubwod\\.h\\.b.*lsx_vsubwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_d_wu:.*vsubwev\\.d\\.wu.*lsx_vsubwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_w_hu:.*vsubwev\\.w\\.hu.*lsx_vsubwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_h_bu:.*vsubwev\\.h\\.bu.*lsx_vsubwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_d_wu:.*vsubwod\\.d\\.wu.*lsx_vsubwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_w_hu:.*vsubwod\\.w\\.hu.*lsx_vsubwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_h_bu:.*vsubwod\\.h\\.bu.*lsx_vsubwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_q_d:.*vaddwev\\.q\\.d.*lsx_vaddwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_q_d:.*vaddwod\\.q\\.d.*lsx_vaddwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_q_du:.*vaddwev\\.q\\.du.*lsx_vaddwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_q_du:.*vaddwod\\.q\\.du.*lsx_vaddwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_q_d:.*vsubwev\\.q\\.d.*lsx_vsubwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_q_d:.*vsubwod\\.q\\.d.*lsx_vsubwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwev_q_du:.*vsubwev\\.q\\.du.*lsx_vsubwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsubwod_q_du:.*vsubwod\\.q\\.du.*lsx_vsubwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwev_q_du_d:.*vaddwev\\.q\\.du\\.d.*lsx_vaddwev_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vaddwod_q_du_d:.*vaddwod\\.q\\.du\\.d.*lsx_vaddwod_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_d_w:.*vmulwev\\.d\\.w.*lsx_vmulwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_w_h:.*vmulwev\\.w\\.h.*lsx_vmulwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_h_b:.*vmulwev\\.h\\.b.*lsx_vmulwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_d_w:.*vmulwod\\.d\\.w.*lsx_vmulwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_w_h:.*vmulwod\\.w\\.h.*lsx_vmulwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_h_b:.*vmulwod\\.h\\.b.*lsx_vmulwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_d_wu:.*vmulwev\\.d\\.wu.*lsx_vmulwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_w_hu:.*vmulwev\\.w\\.hu.*lsx_vmulwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_h_bu:.*vmulwev\\.h\\.bu.*lsx_vmulwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_d_wu:.*vmulwod\\.d\\.wu.*lsx_vmulwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_w_hu:.*vmulwod\\.w\\.hu.*lsx_vmulwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_h_bu:.*vmulwod\\.h\\.bu.*lsx_vmulwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_d_wu_w:.*vmulwev\\.d\\.wu\\.w.*lsx_vmulwev_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_w_hu_h:.*vmulwev\\.w\\.hu\\.h.*lsx_vmulwev_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_h_bu_b:.*vmulwev\\.h\\.bu\\.b.*lsx_vmulwev_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_d_wu_w:.*vmulwod\\.d\\.wu\\.w.*lsx_vmulwod_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_w_hu_h:.*vmulwod\\.w\\.hu\\.h.*lsx_vmulwod_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_h_bu_b:.*vmulwod\\.h\\.bu\\.b.*lsx_vmulwod_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_q_d:.*vmulwev\\.q\\.d.*lsx_vmulwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_q_d:.*vmulwod\\.q\\.d.*lsx_vmulwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_q_du:.*vmulwev\\.q\\.du.*lsx_vmulwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_q_du:.*vmulwod\\.q\\.du.*lsx_vmulwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwev_q_du_d:.*vmulwev\\.q\\.du\\.d.*lsx_vmulwev_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmulwod_q_du_d:.*vmulwod\\.q\\.du\\.d.*lsx_vmulwod_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_q_d:.*vhaddw\\.q\\.d.*lsx_vhaddw_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhaddw_qu_du:.*vhaddw\\.qu\\.du.*lsx_vhaddw_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_q_d:.*vhsubw\\.q\\.d.*lsx_vhsubw_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vhsubw_qu_du:.*vhsubw\\.qu\\.du.*lsx_vhsubw_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_d_w:.*vmaddwev\\.d\\.w.*lsx_vmaddwev_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_w_h:.*vmaddwev\\.w\\.h.*lsx_vmaddwev_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_h_b:.*vmaddwev\\.h\\.b.*lsx_vmaddwev_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_d_wu:.*vmaddwev\\.d\\.wu.*lsx_vmaddwev_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_w_hu:.*vmaddwev\\.w\\.hu.*lsx_vmaddwev_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_h_bu:.*vmaddwev\\.h\\.bu.*lsx_vmaddwev_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_d_w:.*vmaddwod\\.d\\.w.*lsx_vmaddwod_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_w_h:.*vmaddwod\\.w\\.h.*lsx_vmaddwod_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_h_b:.*vmaddwod\\.h\\.b.*lsx_vmaddwod_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_d_wu:.*vmaddwod\\.d\\.wu.*lsx_vmaddwod_d_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_w_hu:.*vmaddwod\\.w\\.hu.*lsx_vmaddwod_w_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_h_bu:.*vmaddwod\\.h\\.bu.*lsx_vmaddwod_h_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_d_wu_w:.*vmaddwev\\.d\\.wu\\.w.*lsx_vmaddwev_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_w_hu_h:.*vmaddwev\\.w\\.hu\\.h.*lsx_vmaddwev_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_h_bu_b:.*vmaddwev\\.h\\.bu\\.b.*lsx_vmaddwev_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_d_wu_w:.*vmaddwod\\.d\\.wu\\.w.*lsx_vmaddwod_d_wu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_w_hu_h:.*vmaddwod\\.w\\.hu\\.h.*lsx_vmaddwod_w_hu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_h_bu_b:.*vmaddwod\\.h\\.bu\\.b.*lsx_vmaddwod_h_bu_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_q_d:.*vmaddwev\\.q\\.d.*lsx_vmaddwev_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_q_d:.*vmaddwod\\.q\\.d.*lsx_vmaddwod_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_q_du:.*vmaddwev\\.q\\.du.*lsx_vmaddwev_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_q_du:.*vmaddwod\\.q\\.du.*lsx_vmaddwod_q_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwev_q_du_d:.*vmaddwev\\.q\\.du\\.d.*lsx_vmaddwev_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmaddwod_q_du_d:.*vmaddwod\\.q\\.du\\.d.*lsx_vmaddwod_q_du_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotr_b:.*vrotr\\.b.*lsx_vrotr_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotr_h:.*vrotr\\.h.*lsx_vrotr_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotr_w:.*vrotr\\.w.*lsx_vrotr_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotr_d:.*vrotr\\.d.*lsx_vrotr_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vadd_q:.*vadd\\.q.*lsx_vadd_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsub_q:.*vsub\\.q.*lsx_vsub_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vldrepl_b:.*vldrepl\\.b.*lsx_vldrepl_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vldrepl_h:.*vldrepl\\.h.*lsx_vldrepl_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vldrepl_w:.*vldrepl\\.w.*lsx_vldrepl_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vldrepl_d:.*vldrepl\\.d.*lsx_vldrepl_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmskgez_b:.*vmskgez\\.b.*lsx_vmskgez_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vmsknz_b:.*vmsknz\\.b.*lsx_vmsknz_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_h_b:.*vexth\\.h\\.b.*lsx_vexth_h_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_w_h:.*vexth\\.w\\.h.*lsx_vexth_w_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_d_w:.*vexth\\.d\\.w.*lsx_vexth_d_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_q_d:.*vexth\\.q\\.d.*lsx_vexth_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_hu_bu:.*vexth\\.hu\\.bu.*lsx_vexth_hu_bu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_wu_hu:.*vexth\\.wu\\.hu.*lsx_vexth_wu_hu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_du_wu:.*vexth\\.du\\.wu.*lsx_vexth_du_wu" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vexth_qu_du:.*vexth\\.qu\\.du.*lsx_vexth_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotri_b:.*vrotri\\.b.*lsx_vrotri_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotri_h:.*vrotri\\.h.*lsx_vrotri_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotri_w:.*vrotri\\.w.*lsx_vrotri_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrotri_d:.*vrotri\\.d.*lsx_vrotri_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vextl_q_d:.*vextl\\.q\\.d.*lsx_vextl_q_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlni_b_h:.*vsrlni\\.b\\.h.*lsx_vsrlni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlni_h_w:.*vsrlni\\.h\\.w.*lsx_vsrlni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlni_w_d:.*vsrlni\\.w\\.d.*lsx_vsrlni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlni_d_q:.*vsrlni\\.d\\.q.*lsx_vsrlni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrni_b_h:.*vsrlrni\\.b\\.h.*lsx_vsrlrni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrni_h_w:.*vsrlrni\\.h\\.w.*lsx_vsrlrni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrni_w_d:.*vsrlrni\\.w\\.d.*lsx_vsrlrni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrlrni_d_q:.*vsrlrni\\.d\\.q.*lsx_vsrlrni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_b_h:.*vssrlni\\.b\\.h.*lsx_vssrlni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_h_w:.*vssrlni\\.h\\.w.*lsx_vssrlni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_w_d:.*vssrlni\\.w\\.d.*lsx_vssrlni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_d_q:.*vssrlni\\.d\\.q.*lsx_vssrlni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_bu_h:.*vssrlni\\.bu\\.h.*lsx_vssrlni_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_hu_w:.*vssrlni\\.hu\\.w.*lsx_vssrlni_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_wu_d:.*vssrlni\\.wu\\.d.*lsx_vssrlni_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlni_du_q:.*vssrlni\\.du\\.q.*lsx_vssrlni_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_b_h:.*vssrlrni\\.b\\.h.*lsx_vssrlrni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_h_w:.*vssrlrni\\.h\\.w.*lsx_vssrlrni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_w_d:.*vssrlrni\\.w\\.d.*lsx_vssrlrni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_d_q:.*vssrlrni\\.d\\.q.*lsx_vssrlrni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_bu_h:.*vssrlrni\\.bu\\.h.*lsx_vssrlrni_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_hu_w:.*vssrlrni\\.hu\\.w.*lsx_vssrlrni_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_wu_d:.*vssrlrni\\.wu\\.d.*lsx_vssrlrni_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrni_du_q:.*vssrlrni\\.du\\.q.*lsx_vssrlrni_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrani_b_h:.*vsrani\\.b\\.h.*lsx_vsrani_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrani_h_w:.*vsrani\\.h\\.w.*lsx_vsrani_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrani_w_d:.*vsrani\\.w\\.d.*lsx_vsrani_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrani_d_q:.*vsrani\\.d\\.q.*lsx_vsrani_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarni_b_h:.*vsrarni\\.b\\.h.*lsx_vsrarni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarni_h_w:.*vsrarni\\.h\\.w.*lsx_vsrarni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarni_w_d:.*vsrarni\\.w\\.d.*lsx_vsrarni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vsrarni_d_q:.*vsrarni\\.d\\.q.*lsx_vsrarni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_b_h:.*vssrani\\.b\\.h.*lsx_vssrani_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_h_w:.*vssrani\\.h\\.w.*lsx_vssrani_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_w_d:.*vssrani\\.w\\.d.*lsx_vssrani_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_d_q:.*vssrani\\.d\\.q.*lsx_vssrani_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_bu_h:.*vssrani\\.bu\\.h.*lsx_vssrani_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_hu_w:.*vssrani\\.hu\\.w.*lsx_vssrani_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_wu_d:.*vssrani\\.wu\\.d.*lsx_vssrani_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrani_du_q:.*vssrani\\.du\\.q.*lsx_vssrani_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_b_h:.*vssrarni\\.b\\.h.*lsx_vssrarni_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_h_w:.*vssrarni\\.h\\.w.*lsx_vssrarni_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_w_d:.*vssrarni\\.w\\.d.*lsx_vssrarni_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_d_q:.*vssrarni\\.d\\.q.*lsx_vssrarni_d_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_bu_h:.*vssrarni\\.bu\\.h.*lsx_vssrarni_bu_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_hu_w:.*vssrarni\\.hu\\.w.*lsx_vssrarni_hu_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_wu_d:.*vssrarni\\.wu\\.d.*lsx_vssrarni_wu_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrarni_du_q:.*vssrarni\\.du\\.q.*lsx_vssrarni_du_q" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vpermi_w:.*vpermi\\.w.*lsx_vpermi_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vld:.*vld.*lsx_vld" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vst:.*vst.*lsx_vst" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrn_b_h:.*vssrlrn\\.b\\.h.*lsx_vssrlrn_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrn_h_w:.*vssrlrn\\.h\\.w.*lsx_vssrlrn_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrlrn_w_d:.*vssrlrn\\.w\\.d.*lsx_vssrlrn_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrln_b_h:.*vssrln\\.b\\.h.*lsx_vssrln_b_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrln_h_w:.*vssrln\\.h\\.w.*lsx_vssrln_h_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vssrln_w_d:.*vssrln\\.w\\.d.*lsx_vssrln_w_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vorn_v:.*vorn\\.v.*lsx_vorn_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vldi:.*vldi.*lsx_vldi" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vshuf_b:.*vshuf\\.b.*lsx_vshuf_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vldx:.*vldx.*lsx_vldx" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vstx:.*vstx.*lsx_vstx" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vextl_qu_du:.*vextl\\.qu\\.du.*lsx_vextl_qu_du" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bnz_b:.*vsetanyeqz\\.b.*lsx_bnz_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bnz_d:.*vsetanyeqz\\.d.*lsx_bnz_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bnz_h:.*vsetanyeqz\\.h.*lsx_bnz_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bnz_v:.*vseteqz\\.v.*lsx_bnz_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bnz_w:.*vsetanyeqz\\.w.*lsx_bnz_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bz_b:.*vsetallnez\\.b.*lsx_bz_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bz_d:.*vsetallnez\\.d.*lsx_bz_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bz_h:.*vsetallnez\\.h.*lsx_bz_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bz_v:.*vsetnez\\.v.*lsx_bz_v" 1 } } */
/* { dg-final { scan-assembler-times "lsx_bz_w:.*vsetallnez\\.w.*lsx_bz_w" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_caf_d:.*vfcmp\\.caf\\.d.*lsx_vfcmp_caf_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_caf_s:.*vfcmp\\.caf\\.s.*lsx_vfcmp_caf_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_ceq_d:.*vfcmp\\.ceq\\.d.*lsx_vfcmp_ceq_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_ceq_s:.*vfcmp\\.ceq\\.s.*lsx_vfcmp_ceq_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cle_d:.*vfcmp\\.cle\\.d.*lsx_vfcmp_cle_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cle_s:.*vfcmp\\.cle\\.s.*lsx_vfcmp_cle_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_clt_d:.*vfcmp\\.clt\\.d.*lsx_vfcmp_clt_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_clt_s:.*vfcmp\\.clt\\.s.*lsx_vfcmp_clt_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cne_d:.*vfcmp\\.cne\\.d.*lsx_vfcmp_cne_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cne_s:.*vfcmp\\.cne\\.s.*lsx_vfcmp_cne_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cor_d:.*vfcmp\\.cor\\.d.*lsx_vfcmp_cor_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cor_s:.*vfcmp\\.cor\\.s.*lsx_vfcmp_cor_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cueq_d:.*vfcmp\\.cueq\\.d.*lsx_vfcmp_cueq_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cueq_s:.*vfcmp\\.cueq\\.s.*lsx_vfcmp_cueq_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cule_d:.*vfcmp\\.cule\\.d.*lsx_vfcmp_cule_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cule_s:.*vfcmp\\.cule\\.s.*lsx_vfcmp_cule_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cult_d:.*vfcmp\\.cult\\.d.*lsx_vfcmp_cult_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cult_s:.*vfcmp\\.cult\\.s.*lsx_vfcmp_cult_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cun_d:.*vfcmp\\.cun\\.d.*lsx_vfcmp_cun_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cune_d:.*vfcmp\\.cune\\.d.*lsx_vfcmp_cune_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cune_s:.*vfcmp\\.cune\\.s.*lsx_vfcmp_cune_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_cun_s:.*vfcmp\\.cun\\.s.*lsx_vfcmp_cun_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_saf_d:.*vfcmp\\.saf\\.d.*lsx_vfcmp_saf_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_saf_s:.*vfcmp\\.saf\\.s.*lsx_vfcmp_saf_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_seq_d:.*vfcmp\\.seq\\.d.*lsx_vfcmp_seq_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_seq_s:.*vfcmp\\.seq\\.s.*lsx_vfcmp_seq_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sle_d:.*vfcmp\\.sle\\.d.*lsx_vfcmp_sle_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sle_s:.*vfcmp\\.sle\\.s.*lsx_vfcmp_sle_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_slt_d:.*vfcmp\\.slt\\.d.*lsx_vfcmp_slt_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_slt_s:.*vfcmp\\.slt\\.s.*lsx_vfcmp_slt_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sne_d:.*vfcmp\\.sne\\.d.*lsx_vfcmp_sne_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sne_s:.*vfcmp\\.sne\\.s.*lsx_vfcmp_sne_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sor_d:.*vfcmp\\.sor\\.d.*lsx_vfcmp_sor_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sor_s:.*vfcmp\\.sor\\.s.*lsx_vfcmp_sor_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sueq_d:.*vfcmp\\.sueq\\.d.*lsx_vfcmp_sueq_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sueq_s:.*vfcmp\\.sueq\\.s.*lsx_vfcmp_sueq_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sule_d:.*vfcmp\\.sule\\.d.*lsx_vfcmp_sule_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sule_s:.*vfcmp\\.sule\\.s.*lsx_vfcmp_sule_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sult_d:.*vfcmp\\.sult\\.d.*lsx_vfcmp_sult_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sult_s:.*vfcmp\\.sult\\.s.*lsx_vfcmp_sult_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sun_d:.*vfcmp\\.sun\\.d.*lsx_vfcmp_sun_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sune_d:.*vfcmp\\.sune\\.d.*lsx_vfcmp_sune_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sune_s:.*vfcmp\\.sune\\.s.*lsx_vfcmp_sune_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfcmp_sun_s:.*vfcmp\\.sun\\.s.*lsx_vfcmp_sun_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrepli_b:.*vrepli\\.b.*lsx_vrepli_b" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrepli_d:.*vrepli\\.d.*lsx_vrepli_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrepli_h:.*vrepli\\.h.*lsx_vrepli_h" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vrepli_w:.*vrepli\\.w.*lsx_vrepli_w" 1 } } */

typedef signed char v16i8 __attribute__ ((vector_size (16), aligned (16)));
typedef signed char v16i8_b __attribute__ ((vector_size (16), aligned (1)));
typedef unsigned char v16u8 __attribute__ ((vector_size (16), aligned (16)));
typedef unsigned char v16u8_b __attribute__ ((vector_size (16), aligned (1)));
typedef short v8i16 __attribute__ ((vector_size (16), aligned (16)));
typedef short v8i16_h __attribute__ ((vector_size (16), aligned (2)));
typedef unsigned short v8u16 __attribute__ ((vector_size (16), aligned (16)));
typedef unsigned short v8u16_h __attribute__ ((vector_size (16), aligned (2)));
typedef int v4i32 __attribute__ ((vector_size (16), aligned (16)));
typedef int v4i32_w __attribute__ ((vector_size (16), aligned (4)));
typedef unsigned int v4u32 __attribute__ ((vector_size (16), aligned (16)));
typedef unsigned int v4u32_w __attribute__ ((vector_size (16), aligned (4)));
typedef long long v2i64 __attribute__ ((vector_size (16), aligned (16)));
typedef long long v2i64_d __attribute__ ((vector_size (16), aligned (8)));
typedef unsigned long long v2u64
    __attribute__ ((vector_size (16), aligned (16)));
typedef unsigned long long v2u64_d
    __attribute__ ((vector_size (16), aligned (8)));
typedef float v4f32 __attribute__ ((vector_size (16), aligned (16)));
typedef float v4f32_w __attribute__ ((vector_size (16), aligned (4)));
typedef double v2f64 __attribute__ ((vector_size (16), aligned (16)));
typedef double v2f64_d __attribute__ ((vector_size (16), aligned (8)));

typedef long long __m128i
    __attribute__ ((__vector_size__ (16), __may_alias__));
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

v16i8
__lsx_vsll_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsll_b (_1, _2);
}
v8i16
__lsx_vsll_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsll_h (_1, _2);
}
v4i32
__lsx_vsll_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsll_w (_1, _2);
}
v2i64
__lsx_vsll_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsll_d (_1, _2);
}
v16i8
__lsx_vslli_b (v16i8 _1)
{
  return __builtin_lsx_vslli_b (_1, 1);
}
v8i16
__lsx_vslli_h (v8i16 _1)
{
  return __builtin_lsx_vslli_h (_1, 1);
}
v4i32
__lsx_vslli_w (v4i32 _1)
{
  return __builtin_lsx_vslli_w (_1, 1);
}
v2i64
__lsx_vslli_d (v2i64 _1)
{
  return __builtin_lsx_vslli_d (_1, 1);
}
v16i8
__lsx_vsra_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsra_b (_1, _2);
}
v8i16
__lsx_vsra_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsra_h (_1, _2);
}
v4i32
__lsx_vsra_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsra_w (_1, _2);
}
v2i64
__lsx_vsra_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsra_d (_1, _2);
}
v16i8
__lsx_vsrai_b (v16i8 _1)
{
  return __builtin_lsx_vsrai_b (_1, 1);
}
v8i16
__lsx_vsrai_h (v8i16 _1)
{
  return __builtin_lsx_vsrai_h (_1, 1);
}
v4i32
__lsx_vsrai_w (v4i32 _1)
{
  return __builtin_lsx_vsrai_w (_1, 1);
}
v2i64
__lsx_vsrai_d (v2i64 _1)
{
  return __builtin_lsx_vsrai_d (_1, 1);
}
v16i8
__lsx_vsrar_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrar_b (_1, _2);
}
v8i16
__lsx_vsrar_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrar_h (_1, _2);
}
v4i32
__lsx_vsrar_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrar_w (_1, _2);
}
v2i64
__lsx_vsrar_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrar_d (_1, _2);
}
v16i8
__lsx_vsrari_b (v16i8 _1)
{
  return __builtin_lsx_vsrari_b (_1, 1);
}
v8i16
__lsx_vsrari_h (v8i16 _1)
{
  return __builtin_lsx_vsrari_h (_1, 1);
}
v4i32
__lsx_vsrari_w (v4i32 _1)
{
  return __builtin_lsx_vsrari_w (_1, 1);
}
v2i64
__lsx_vsrari_d (v2i64 _1)
{
  return __builtin_lsx_vsrari_d (_1, 1);
}
v16i8
__lsx_vsrl_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrl_b (_1, _2);
}
v8i16
__lsx_vsrl_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrl_h (_1, _2);
}
v4i32
__lsx_vsrl_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrl_w (_1, _2);
}
v2i64
__lsx_vsrl_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrl_d (_1, _2);
}
v16i8
__lsx_vsrli_b (v16i8 _1)
{
  return __builtin_lsx_vsrli_b (_1, 1);
}
v8i16
__lsx_vsrli_h (v8i16 _1)
{
  return __builtin_lsx_vsrli_h (_1, 1);
}
v4i32
__lsx_vsrli_w (v4i32 _1)
{
  return __builtin_lsx_vsrli_w (_1, 1);
}
v2i64
__lsx_vsrli_d (v2i64 _1)
{
  return __builtin_lsx_vsrli_d (_1, 1);
}
v16i8
__lsx_vsrlr_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrlr_b (_1, _2);
}
v8i16
__lsx_vsrlr_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrlr_h (_1, _2);
}
v4i32
__lsx_vsrlr_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrlr_w (_1, _2);
}
v2i64
__lsx_vsrlr_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrlr_d (_1, _2);
}
v16i8
__lsx_vsrlri_b (v16i8 _1)
{
  return __builtin_lsx_vsrlri_b (_1, 1);
}
v8i16
__lsx_vsrlri_h (v8i16 _1)
{
  return __builtin_lsx_vsrlri_h (_1, 1);
}
v4i32
__lsx_vsrlri_w (v4i32 _1)
{
  return __builtin_lsx_vsrlri_w (_1, 1);
}
v2i64
__lsx_vsrlri_d (v2i64 _1)
{
  return __builtin_lsx_vsrlri_d (_1, 1);
}
v16u8
__lsx_vbitclr_b (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vbitclr_b (_1, _2);
}
v8u16
__lsx_vbitclr_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vbitclr_h (_1, _2);
}
v4u32
__lsx_vbitclr_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vbitclr_w (_1, _2);
}
v2u64
__lsx_vbitclr_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vbitclr_d (_1, _2);
}
v16u8
__lsx_vbitclri_b (v16u8 _1)
{
  return __builtin_lsx_vbitclri_b (_1, 1);
}
v8u16
__lsx_vbitclri_h (v8u16 _1)
{
  return __builtin_lsx_vbitclri_h (_1, 1);
}
v4u32
__lsx_vbitclri_w (v4u32 _1)
{
  return __builtin_lsx_vbitclri_w (_1, 1);
}
v2u64
__lsx_vbitclri_d (v2u64 _1)
{
  return __builtin_lsx_vbitclri_d (_1, 1);
}
v16u8
__lsx_vbitset_b (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vbitset_b (_1, _2);
}
v8u16
__lsx_vbitset_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vbitset_h (_1, _2);
}
v4u32
__lsx_vbitset_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vbitset_w (_1, _2);
}
v2u64
__lsx_vbitset_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vbitset_d (_1, _2);
}
v16u8
__lsx_vbitseti_b (v16u8 _1)
{
  return __builtin_lsx_vbitseti_b (_1, 1);
}
v8u16
__lsx_vbitseti_h (v8u16 _1)
{
  return __builtin_lsx_vbitseti_h (_1, 1);
}
v4u32
__lsx_vbitseti_w (v4u32 _1)
{
  return __builtin_lsx_vbitseti_w (_1, 1);
}
v2u64
__lsx_vbitseti_d (v2u64 _1)
{
  return __builtin_lsx_vbitseti_d (_1, 1);
}
v16u8
__lsx_vbitrev_b (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vbitrev_b (_1, _2);
}
v8u16
__lsx_vbitrev_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vbitrev_h (_1, _2);
}
v4u32
__lsx_vbitrev_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vbitrev_w (_1, _2);
}
v2u64
__lsx_vbitrev_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vbitrev_d (_1, _2);
}
v16u8
__lsx_vbitrevi_b (v16u8 _1)
{
  return __builtin_lsx_vbitrevi_b (_1, 1);
}
v8u16
__lsx_vbitrevi_h (v8u16 _1)
{
  return __builtin_lsx_vbitrevi_h (_1, 1);
}
v4u32
__lsx_vbitrevi_w (v4u32 _1)
{
  return __builtin_lsx_vbitrevi_w (_1, 1);
}
v2u64
__lsx_vbitrevi_d (v2u64 _1)
{
  return __builtin_lsx_vbitrevi_d (_1, 1);
}
v16i8
__lsx_vadd_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vadd_b (_1, _2);
}
v8i16
__lsx_vadd_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vadd_h (_1, _2);
}
v4i32
__lsx_vadd_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vadd_w (_1, _2);
}
v2i64
__lsx_vadd_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vadd_d (_1, _2);
}
v16i8
__lsx_vaddi_bu (v16i8 _1)
{
  return __builtin_lsx_vaddi_bu (_1, 1);
}
v8i16
__lsx_vaddi_hu (v8i16 _1)
{
  return __builtin_lsx_vaddi_hu (_1, 1);
}
v4i32
__lsx_vaddi_wu (v4i32 _1)
{
  return __builtin_lsx_vaddi_wu (_1, 1);
}
v2i64
__lsx_vaddi_du (v2i64 _1)
{
  return __builtin_lsx_vaddi_du (_1, 1);
}
v16i8
__lsx_vsub_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsub_b (_1, _2);
}
v8i16
__lsx_vsub_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsub_h (_1, _2);
}
v4i32
__lsx_vsub_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsub_w (_1, _2);
}
v2i64
__lsx_vsub_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsub_d (_1, _2);
}
v16i8
__lsx_vsubi_bu (v16i8 _1)
{
  return __builtin_lsx_vsubi_bu (_1, 1);
}
v8i16
__lsx_vsubi_hu (v8i16 _1)
{
  return __builtin_lsx_vsubi_hu (_1, 1);
}
v4i32
__lsx_vsubi_wu (v4i32 _1)
{
  return __builtin_lsx_vsubi_wu (_1, 1);
}
v2i64
__lsx_vsubi_du (v2i64 _1)
{
  return __builtin_lsx_vsubi_du (_1, 1);
}
v16i8
__lsx_vmax_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmax_b (_1, _2);
}
v8i16
__lsx_vmax_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmax_h (_1, _2);
}
v4i32
__lsx_vmax_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmax_w (_1, _2);
}
v2i64
__lsx_vmax_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmax_d (_1, _2);
}
v16i8
__lsx_vmaxi_b (v16i8 _1)
{
  return __builtin_lsx_vmaxi_b (_1, 1);
}
v8i16
__lsx_vmaxi_h (v8i16 _1)
{
  return __builtin_lsx_vmaxi_h (_1, 1);
}
v4i32
__lsx_vmaxi_w (v4i32 _1)
{
  return __builtin_lsx_vmaxi_w (_1, 1);
}
v2i64
__lsx_vmaxi_d (v2i64 _1)
{
  return __builtin_lsx_vmaxi_d (_1, 1);
}
v16u8
__lsx_vmax_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vmax_bu (_1, _2);
}
v8u16
__lsx_vmax_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vmax_hu (_1, _2);
}
v4u32
__lsx_vmax_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vmax_wu (_1, _2);
}
v2u64
__lsx_vmax_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vmax_du (_1, _2);
}
v16u8
__lsx_vmaxi_bu (v16u8 _1)
{
  return __builtin_lsx_vmaxi_bu (_1, 1);
}
v8u16
__lsx_vmaxi_hu (v8u16 _1)
{
  return __builtin_lsx_vmaxi_hu (_1, 1);
}
v4u32
__lsx_vmaxi_wu (v4u32 _1)
{
  return __builtin_lsx_vmaxi_wu (_1, 1);
}
v2u64
__lsx_vmaxi_du (v2u64 _1)
{
  return __builtin_lsx_vmaxi_du (_1, 1);
}
v16i8
__lsx_vmin_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmin_b (_1, _2);
}
v8i16
__lsx_vmin_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmin_h (_1, _2);
}
v4i32
__lsx_vmin_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmin_w (_1, _2);
}
v2i64
__lsx_vmin_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmin_d (_1, _2);
}
v16i8
__lsx_vmini_b (v16i8 _1)
{
  return __builtin_lsx_vmini_b (_1, 1);
}
v8i16
__lsx_vmini_h (v8i16 _1)
{
  return __builtin_lsx_vmini_h (_1, 1);
}
v4i32
__lsx_vmini_w (v4i32 _1)
{
  return __builtin_lsx_vmini_w (_1, 1);
}
v2i64
__lsx_vmini_d (v2i64 _1)
{
  return __builtin_lsx_vmini_d (_1, 1);
}
v16u8
__lsx_vmin_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vmin_bu (_1, _2);
}
v8u16
__lsx_vmin_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vmin_hu (_1, _2);
}
v4u32
__lsx_vmin_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vmin_wu (_1, _2);
}
v2u64
__lsx_vmin_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vmin_du (_1, _2);
}
v16u8
__lsx_vmini_bu (v16u8 _1)
{
  return __builtin_lsx_vmini_bu (_1, 1);
}
v8u16
__lsx_vmini_hu (v8u16 _1)
{
  return __builtin_lsx_vmini_hu (_1, 1);
}
v4u32
__lsx_vmini_wu (v4u32 _1)
{
  return __builtin_lsx_vmini_wu (_1, 1);
}
v2u64
__lsx_vmini_du (v2u64 _1)
{
  return __builtin_lsx_vmini_du (_1, 1);
}
v16i8
__lsx_vseq_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vseq_b (_1, _2);
}
v8i16
__lsx_vseq_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vseq_h (_1, _2);
}
v4i32
__lsx_vseq_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vseq_w (_1, _2);
}
v2i64
__lsx_vseq_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vseq_d (_1, _2);
}
v16i8
__lsx_vseqi_b (v16i8 _1)
{
  return __builtin_lsx_vseqi_b (_1, 1);
}
v8i16
__lsx_vseqi_h (v8i16 _1)
{
  return __builtin_lsx_vseqi_h (_1, 1);
}
v4i32
__lsx_vseqi_w (v4i32 _1)
{
  return __builtin_lsx_vseqi_w (_1, 1);
}
v2i64
__lsx_vseqi_d (v2i64 _1)
{
  return __builtin_lsx_vseqi_d (_1, 1);
}
v16i8
__lsx_vslti_b (v16i8 _1)
{
  return __builtin_lsx_vslti_b (_1, 1);
}
v16i8
__lsx_vslt_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vslt_b (_1, _2);
}
v8i16
__lsx_vslt_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vslt_h (_1, _2);
}
v4i32
__lsx_vslt_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vslt_w (_1, _2);
}
v2i64
__lsx_vslt_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vslt_d (_1, _2);
}
v8i16
__lsx_vslti_h (v8i16 _1)
{
  return __builtin_lsx_vslti_h (_1, 1);
}
v4i32
__lsx_vslti_w (v4i32 _1)
{
  return __builtin_lsx_vslti_w (_1, 1);
}
v2i64
__lsx_vslti_d (v2i64 _1)
{
  return __builtin_lsx_vslti_d (_1, 1);
}
v16i8
__lsx_vslt_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vslt_bu (_1, _2);
}
v8i16
__lsx_vslt_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vslt_hu (_1, _2);
}
v4i32
__lsx_vslt_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vslt_wu (_1, _2);
}
v2i64
__lsx_vslt_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vslt_du (_1, _2);
}
v16i8
__lsx_vslti_bu (v16u8 _1)
{
  return __builtin_lsx_vslti_bu (_1, 1);
}
v8i16
__lsx_vslti_hu (v8u16 _1)
{
  return __builtin_lsx_vslti_hu (_1, 1);
}
v4i32
__lsx_vslti_wu (v4u32 _1)
{
  return __builtin_lsx_vslti_wu (_1, 1);
}
v2i64
__lsx_vslti_du (v2u64 _1)
{
  return __builtin_lsx_vslti_du (_1, 1);
}
v16i8
__lsx_vsle_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsle_b (_1, _2);
}
v8i16
__lsx_vsle_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsle_h (_1, _2);
}
v4i32
__lsx_vsle_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsle_w (_1, _2);
}
v2i64
__lsx_vsle_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsle_d (_1, _2);
}
v16i8
__lsx_vslei_b (v16i8 _1)
{
  return __builtin_lsx_vslei_b (_1, 1);
}
v8i16
__lsx_vslei_h (v8i16 _1)
{
  return __builtin_lsx_vslei_h (_1, 1);
}
v4i32
__lsx_vslei_w (v4i32 _1)
{
  return __builtin_lsx_vslei_w (_1, 1);
}
v2i64
__lsx_vslei_d (v2i64 _1)
{
  return __builtin_lsx_vslei_d (_1, 1);
}
v16i8
__lsx_vsle_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vsle_bu (_1, _2);
}
v8i16
__lsx_vsle_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vsle_hu (_1, _2);
}
v4i32
__lsx_vsle_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vsle_wu (_1, _2);
}
v2i64
__lsx_vsle_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vsle_du (_1, _2);
}
v16i8
__lsx_vslei_bu (v16u8 _1)
{
  return __builtin_lsx_vslei_bu (_1, 1);
}
v8i16
__lsx_vslei_hu (v8u16 _1)
{
  return __builtin_lsx_vslei_hu (_1, 1);
}
v4i32
__lsx_vslei_wu (v4u32 _1)
{
  return __builtin_lsx_vslei_wu (_1, 1);
}
v2i64
__lsx_vslei_du (v2u64 _1)
{
  return __builtin_lsx_vslei_du (_1, 1);
}
v16i8
__lsx_vsat_b (v16i8 _1)
{
  return __builtin_lsx_vsat_b (_1, 1);
}
v8i16
__lsx_vsat_h (v8i16 _1)
{
  return __builtin_lsx_vsat_h (_1, 1);
}
v4i32
__lsx_vsat_w (v4i32 _1)
{
  return __builtin_lsx_vsat_w (_1, 1);
}
v2i64
__lsx_vsat_d (v2i64 _1)
{
  return __builtin_lsx_vsat_d (_1, 1);
}
v16u8
__lsx_vsat_bu (v16u8 _1)
{
  return __builtin_lsx_vsat_bu (_1, 1);
}
v8u16
__lsx_vsat_hu (v8u16 _1)
{
  return __builtin_lsx_vsat_hu (_1, 1);
}
v4u32
__lsx_vsat_wu (v4u32 _1)
{
  return __builtin_lsx_vsat_wu (_1, 1);
}
v2u64
__lsx_vsat_du (v2u64 _1)
{
  return __builtin_lsx_vsat_du (_1, 1);
}
v16i8
__lsx_vadda_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vadda_b (_1, _2);
}
v8i16
__lsx_vadda_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vadda_h (_1, _2);
}
v4i32
__lsx_vadda_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vadda_w (_1, _2);
}
v2i64
__lsx_vadda_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vadda_d (_1, _2);
}
v16i8
__lsx_vsadd_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsadd_b (_1, _2);
}
v8i16
__lsx_vsadd_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsadd_h (_1, _2);
}
v4i32
__lsx_vsadd_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsadd_w (_1, _2);
}
v2i64
__lsx_vsadd_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsadd_d (_1, _2);
}
v16u8
__lsx_vsadd_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vsadd_bu (_1, _2);
}
v8u16
__lsx_vsadd_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vsadd_hu (_1, _2);
}
v4u32
__lsx_vsadd_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vsadd_wu (_1, _2);
}
v2u64
__lsx_vsadd_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vsadd_du (_1, _2);
}
v16i8
__lsx_vavg_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vavg_b (_1, _2);
}
v8i16
__lsx_vavg_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vavg_h (_1, _2);
}
v4i32
__lsx_vavg_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vavg_w (_1, _2);
}
v2i64
__lsx_vavg_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vavg_d (_1, _2);
}
v16u8
__lsx_vavg_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vavg_bu (_1, _2);
}
v8u16
__lsx_vavg_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vavg_hu (_1, _2);
}
v4u32
__lsx_vavg_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vavg_wu (_1, _2);
}
v2u64
__lsx_vavg_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vavg_du (_1, _2);
}
v16i8
__lsx_vavgr_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vavgr_b (_1, _2);
}
v8i16
__lsx_vavgr_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vavgr_h (_1, _2);
}
v4i32
__lsx_vavgr_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vavgr_w (_1, _2);
}
v2i64
__lsx_vavgr_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vavgr_d (_1, _2);
}
v16u8
__lsx_vavgr_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vavgr_bu (_1, _2);
}
v8u16
__lsx_vavgr_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vavgr_hu (_1, _2);
}
v4u32
__lsx_vavgr_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vavgr_wu (_1, _2);
}
v2u64
__lsx_vavgr_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vavgr_du (_1, _2);
}
v16i8
__lsx_vssub_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vssub_b (_1, _2);
}
v8i16
__lsx_vssub_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssub_h (_1, _2);
}
v4i32
__lsx_vssub_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssub_w (_1, _2);
}
v2i64
__lsx_vssub_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssub_d (_1, _2);
}
v16u8
__lsx_vssub_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vssub_bu (_1, _2);
}
v8u16
__lsx_vssub_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vssub_hu (_1, _2);
}
v4u32
__lsx_vssub_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vssub_wu (_1, _2);
}
v2u64
__lsx_vssub_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vssub_du (_1, _2);
}
v16i8
__lsx_vabsd_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vabsd_b (_1, _2);
}
v8i16
__lsx_vabsd_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vabsd_h (_1, _2);
}
v4i32
__lsx_vabsd_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vabsd_w (_1, _2);
}
v2i64
__lsx_vabsd_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vabsd_d (_1, _2);
}
v16u8
__lsx_vabsd_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vabsd_bu (_1, _2);
}
v8u16
__lsx_vabsd_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vabsd_hu (_1, _2);
}
v4u32
__lsx_vabsd_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vabsd_wu (_1, _2);
}
v2u64
__lsx_vabsd_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vabsd_du (_1, _2);
}
v16i8
__lsx_vmul_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmul_b (_1, _2);
}
v8i16
__lsx_vmul_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmul_h (_1, _2);
}
v4i32
__lsx_vmul_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmul_w (_1, _2);
}
v2i64
__lsx_vmul_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmul_d (_1, _2);
}
v16i8
__lsx_vmadd_b (v16i8 _1, v16i8 _2, v16i8 _3)
{
  return __builtin_lsx_vmadd_b (_1, _2, _3);
}
v8i16
__lsx_vmadd_h (v8i16 _1, v8i16 _2, v8i16 _3)
{
  return __builtin_lsx_vmadd_h (_1, _2, _3);
}
v4i32
__lsx_vmadd_w (v4i32 _1, v4i32 _2, v4i32 _3)
{
  return __builtin_lsx_vmadd_w (_1, _2, _3);
}
v2i64
__lsx_vmadd_d (v2i64 _1, v2i64 _2, v2i64 _3)
{
  return __builtin_lsx_vmadd_d (_1, _2, _3);
}
v16i8
__lsx_vmsub_b (v16i8 _1, v16i8 _2, v16i8 _3)
{
  return __builtin_lsx_vmsub_b (_1, _2, _3);
}
v8i16
__lsx_vmsub_h (v8i16 _1, v8i16 _2, v8i16 _3)
{
  return __builtin_lsx_vmsub_h (_1, _2, _3);
}
v4i32
__lsx_vmsub_w (v4i32 _1, v4i32 _2, v4i32 _3)
{
  return __builtin_lsx_vmsub_w (_1, _2, _3);
}
v2i64
__lsx_vmsub_d (v2i64 _1, v2i64 _2, v2i64 _3)
{
  return __builtin_lsx_vmsub_d (_1, _2, _3);
}
v16i8
__lsx_vdiv_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vdiv_b (_1, _2);
}
v8i16
__lsx_vdiv_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vdiv_h (_1, _2);
}
v4i32
__lsx_vdiv_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vdiv_w (_1, _2);
}
v2i64
__lsx_vdiv_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vdiv_d (_1, _2);
}
v16u8
__lsx_vdiv_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vdiv_bu (_1, _2);
}
v8u16
__lsx_vdiv_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vdiv_hu (_1, _2);
}
v4u32
__lsx_vdiv_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vdiv_wu (_1, _2);
}
v2u64
__lsx_vdiv_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vdiv_du (_1, _2);
}
v8i16
__lsx_vhaddw_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vhaddw_h_b (_1, _2);
}
v4i32
__lsx_vhaddw_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vhaddw_w_h (_1, _2);
}
v2i64
__lsx_vhaddw_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vhaddw_d_w (_1, _2);
}
v8u16
__lsx_vhaddw_hu_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vhaddw_hu_bu (_1, _2);
}
v4u32
__lsx_vhaddw_wu_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vhaddw_wu_hu (_1, _2);
}
v2u64
__lsx_vhaddw_du_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vhaddw_du_wu (_1, _2);
}
v8i16
__lsx_vhsubw_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vhsubw_h_b (_1, _2);
}
v4i32
__lsx_vhsubw_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vhsubw_w_h (_1, _2);
}
v2i64
__lsx_vhsubw_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vhsubw_d_w (_1, _2);
}
v8i16
__lsx_vhsubw_hu_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vhsubw_hu_bu (_1, _2);
}
v4i32
__lsx_vhsubw_wu_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vhsubw_wu_hu (_1, _2);
}
v2i64
__lsx_vhsubw_du_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vhsubw_du_wu (_1, _2);
}
v16i8
__lsx_vmod_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmod_b (_1, _2);
}
v8i16
__lsx_vmod_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmod_h (_1, _2);
}
v4i32
__lsx_vmod_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmod_w (_1, _2);
}
v2i64
__lsx_vmod_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmod_d (_1, _2);
}
v16u8
__lsx_vmod_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vmod_bu (_1, _2);
}
v8u16
__lsx_vmod_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vmod_hu (_1, _2);
}
v4u32
__lsx_vmod_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vmod_wu (_1, _2);
}
v2u64
__lsx_vmod_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vmod_du (_1, _2);
}
v16i8
__lsx_vreplve_b (v16i8 _1, int _2)
{
  return __builtin_lsx_vreplve_b (_1, _2);
}
v8i16
__lsx_vreplve_h (v8i16 _1, int _2)
{
  return __builtin_lsx_vreplve_h (_1, _2);
}
v4i32
__lsx_vreplve_w (v4i32 _1, int _2)
{
  return __builtin_lsx_vreplve_w (_1, _2);
}
v2i64
__lsx_vreplve_d (v2i64 _1, int _2)
{
  return __builtin_lsx_vreplve_d (_1, _2);
}
v16i8
__lsx_vreplvei_b (v16i8 _1)
{
  return __builtin_lsx_vreplvei_b (_1, 1);
}
v8i16
__lsx_vreplvei_h (v8i16 _1)
{
  return __builtin_lsx_vreplvei_h (_1, 1);
}
v4i32
__lsx_vreplvei_w (v4i32 _1)
{
  return __builtin_lsx_vreplvei_w (_1, 1);
}
v2i64
__lsx_vreplvei_d (v2i64 _1)
{
  return __builtin_lsx_vreplvei_d (_1, 1);
}
v16i8
__lsx_vpickev_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vpickev_b (_1, _2);
}
v8i16
__lsx_vpickev_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vpickev_h (_1, _2);
}
v4i32
__lsx_vpickev_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vpickev_w (_1, _2);
}
v2i64
__lsx_vpickev_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vpickev_d (_1, _2);
}
v16i8
__lsx_vpickod_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vpickod_b (_1, _2);
}
v8i16
__lsx_vpickod_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vpickod_h (_1, _2);
}
v4i32
__lsx_vpickod_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vpickod_w (_1, _2);
}
v2i64
__lsx_vpickod_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vpickod_d (_1, _2);
}
v16i8
__lsx_vilvh_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vilvh_b (_1, _2);
}
v8i16
__lsx_vilvh_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vilvh_h (_1, _2);
}
v4i32
__lsx_vilvh_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vilvh_w (_1, _2);
}
v2i64
__lsx_vilvh_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vilvh_d (_1, _2);
}
v16i8
__lsx_vilvl_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vilvl_b (_1, _2);
}
v8i16
__lsx_vilvl_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vilvl_h (_1, _2);
}
v4i32
__lsx_vilvl_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vilvl_w (_1, _2);
}
v2i64
__lsx_vilvl_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vilvl_d (_1, _2);
}
v16i8
__lsx_vpackev_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vpackev_b (_1, _2);
}
v8i16
__lsx_vpackev_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vpackev_h (_1, _2);
}
v4i32
__lsx_vpackev_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vpackev_w (_1, _2);
}
v2i64
__lsx_vpackev_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vpackev_d (_1, _2);
}
v16i8
__lsx_vpackod_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vpackod_b (_1, _2);
}
v8i16
__lsx_vpackod_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vpackod_h (_1, _2);
}
v4i32
__lsx_vpackod_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vpackod_w (_1, _2);
}
v2i64
__lsx_vpackod_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vpackod_d (_1, _2);
}
v8i16
__lsx_vshuf_h (v8i16 _1, v8i16 _2, v8i16 _3)
{
  return __builtin_lsx_vshuf_h (_1, _2, _3);
}
v4i32
__lsx_vshuf_w (v4i32 _1, v4i32 _2, v4i32 _3)
{
  return __builtin_lsx_vshuf_w (_1, _2, _3);
}
v2i64
__lsx_vshuf_d (v2i64 _1, v2i64 _2, v2i64 _3)
{
  return __builtin_lsx_vshuf_d (_1, _2, _3);
}
v16u8
__lsx_vand_v (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vand_v (_1, _2);
}
v16u8
__lsx_vandi_b (v16u8 _1)
{
  return __builtin_lsx_vandi_b (_1, 1);
}
v16u8
__lsx_vor_v (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vor_v (_1, _2);
}
v16u8
__lsx_vori_b (v16u8 _1)
{
  return __builtin_lsx_vori_b (_1, 1);
}
v16u8
__lsx_vnor_v (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vnor_v (_1, _2);
}
v16u8
__lsx_vnori_b (v16u8 _1)
{
  return __builtin_lsx_vnori_b (_1, 1);
}
v16u8
__lsx_vxor_v (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vxor_v (_1, _2);
}
v16u8
__lsx_vxori_b (v16u8 _1)
{
  return __builtin_lsx_vxori_b (_1, 1);
}
v16u8
__lsx_vbitsel_v (v16u8 _1, v16u8 _2, v16u8 _3)
{
  return __builtin_lsx_vbitsel_v (_1, _2, _3);
}
v16u8
__lsx_vbitseli_b (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vbitseli_b (_1, _2, 1);
}
v16i8
__lsx_vshuf4i_b (v16i8 _1)
{
  return __builtin_lsx_vshuf4i_b (_1, 1);
}
v8i16
__lsx_vshuf4i_h (v8i16 _1)
{
  return __builtin_lsx_vshuf4i_h (_1, 1);
}
v4i32
__lsx_vshuf4i_w (v4i32 _1)
{
  return __builtin_lsx_vshuf4i_w (_1, 1);
}
v16i8
__lsx_vreplgr2vr_b (int _1)
{
  return __builtin_lsx_vreplgr2vr_b (_1);
}
v8i16
__lsx_vreplgr2vr_h (int _1)
{
  return __builtin_lsx_vreplgr2vr_h (_1);
}
v4i32
__lsx_vreplgr2vr_w (int _1)
{
  return __builtin_lsx_vreplgr2vr_w (_1);
}
v2i64
__lsx_vreplgr2vr_d (long _1)
{
  return __builtin_lsx_vreplgr2vr_d (_1);
}
v16i8
__lsx_vpcnt_b (v16i8 _1)
{
  return __builtin_lsx_vpcnt_b (_1);
}
v8i16
__lsx_vpcnt_h (v8i16 _1)
{
  return __builtin_lsx_vpcnt_h (_1);
}
v4i32
__lsx_vpcnt_w (v4i32 _1)
{
  return __builtin_lsx_vpcnt_w (_1);
}
v2i64
__lsx_vpcnt_d (v2i64 _1)
{
  return __builtin_lsx_vpcnt_d (_1);
}
v16i8
__lsx_vclo_b (v16i8 _1)
{
  return __builtin_lsx_vclo_b (_1);
}
v8i16
__lsx_vclo_h (v8i16 _1)
{
  return __builtin_lsx_vclo_h (_1);
}
v4i32
__lsx_vclo_w (v4i32 _1)
{
  return __builtin_lsx_vclo_w (_1);
}
v2i64
__lsx_vclo_d (v2i64 _1)
{
  return __builtin_lsx_vclo_d (_1);
}
v16i8
__lsx_vclz_b (v16i8 _1)
{
  return __builtin_lsx_vclz_b (_1);
}
v8i16
__lsx_vclz_h (v8i16 _1)
{
  return __builtin_lsx_vclz_h (_1);
}
v4i32
__lsx_vclz_w (v4i32 _1)
{
  return __builtin_lsx_vclz_w (_1);
}
v2i64
__lsx_vclz_d (v2i64 _1)
{
  return __builtin_lsx_vclz_d (_1);
}
int
__lsx_vpickve2gr_b (v16i8 _1)
{
  return __builtin_lsx_vpickve2gr_b (_1, 1);
}
int
__lsx_vpickve2gr_h (v8i16 _1)
{
  return __builtin_lsx_vpickve2gr_h (_1, 1);
}
int
__lsx_vpickve2gr_w (v4i32 _1)
{
  return __builtin_lsx_vpickve2gr_w (_1, 1);
}
long
__lsx_vpickve2gr_d (v2i64 _1)
{
  return __builtin_lsx_vpickve2gr_d (_1, 1);
}
unsigned int
__lsx_vpickve2gr_bu (v16i8 _1)
{
  return __builtin_lsx_vpickve2gr_bu (_1, 1);
}
unsigned int
__lsx_vpickve2gr_hu (v8i16 _1)
{
  return __builtin_lsx_vpickve2gr_hu (_1, 1);
}
unsigned int
__lsx_vpickve2gr_wu (v4i32 _1)
{
  return __builtin_lsx_vpickve2gr_wu (_1, 1);
}
unsigned long int
__lsx_vpickve2gr_du (v2i64 _1)
{
  return __builtin_lsx_vpickve2gr_du (_1, 1);
}
v16i8
__lsx_vinsgr2vr_b (v16i8 _1)
{
  return __builtin_lsx_vinsgr2vr_b (_1, 1, 1);
}
v8i16
__lsx_vinsgr2vr_h (v8i16 _1)
{
  return __builtin_lsx_vinsgr2vr_h (_1, 1, 1);
}
v4i32
__lsx_vinsgr2vr_w (v4i32 _1)
{
  return __builtin_lsx_vinsgr2vr_w (_1, 1, 1);
}
v2i64
__lsx_vinsgr2vr_d (v2i64 _1)
{
  return __builtin_lsx_vinsgr2vr_d (_1, 1, 1);
}
v4f32
__lsx_vfadd_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfadd_s (_1, _2);
}
v2f64
__lsx_vfadd_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfadd_d (_1, _2);
}
v4f32
__lsx_vfsub_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfsub_s (_1, _2);
}
v2f64
__lsx_vfsub_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfsub_d (_1, _2);
}
v4f32
__lsx_vfmul_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfmul_s (_1, _2);
}
v2f64
__lsx_vfmul_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfmul_d (_1, _2);
}
v4f32
__lsx_vfdiv_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfdiv_s (_1, _2);
}
v2f64
__lsx_vfdiv_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfdiv_d (_1, _2);
}
v8i16
__lsx_vfcvt_h_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcvt_h_s (_1, _2);
}
v4f32
__lsx_vfcvt_s_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcvt_s_d (_1, _2);
}
v4f32
__lsx_vfmin_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfmin_s (_1, _2);
}
v2f64
__lsx_vfmin_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfmin_d (_1, _2);
}
v4f32
__lsx_vfmina_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfmina_s (_1, _2);
}
v2f64
__lsx_vfmina_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfmina_d (_1, _2);
}
v4f32
__lsx_vfmax_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfmax_s (_1, _2);
}
v2f64
__lsx_vfmax_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfmax_d (_1, _2);
}
v4f32
__lsx_vfmaxa_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfmaxa_s (_1, _2);
}
v2f64
__lsx_vfmaxa_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfmaxa_d (_1, _2);
}
v4i32
__lsx_vfclass_s (v4f32 _1)
{
  return __builtin_lsx_vfclass_s (_1);
}
v2i64
__lsx_vfclass_d (v2f64 _1)
{
  return __builtin_lsx_vfclass_d (_1);
}
v4f32
__lsx_vfsqrt_s (v4f32 _1)
{
  return __builtin_lsx_vfsqrt_s (_1);
}
v2f64
__lsx_vfsqrt_d (v2f64 _1)
{
  return __builtin_lsx_vfsqrt_d (_1);
}
v4f32
__lsx_vfrecip_s (v4f32 _1)
{
  return __builtin_lsx_vfrecip_s (_1);
}
v2f64
__lsx_vfrecip_d (v2f64 _1)
{
  return __builtin_lsx_vfrecip_d (_1);
}
v4f32
__lsx_vfrint_s (v4f32 _1)
{
  return __builtin_lsx_vfrint_s (_1);
}
v2f64
__lsx_vfrint_d (v2f64 _1)
{
  return __builtin_lsx_vfrint_d (_1);
}
v4f32
__lsx_vfrsqrt_s (v4f32 _1)
{
  return __builtin_lsx_vfrsqrt_s (_1);
}
v2f64
__lsx_vfrsqrt_d (v2f64 _1)
{
  return __builtin_lsx_vfrsqrt_d (_1);
}
v4f32
__lsx_vflogb_s (v4f32 _1)
{
  return __builtin_lsx_vflogb_s (_1);
}
v2f64
__lsx_vflogb_d (v2f64 _1)
{
  return __builtin_lsx_vflogb_d (_1);
}
v4f32
__lsx_vfcvth_s_h (v8i16 _1)
{
  return __builtin_lsx_vfcvth_s_h (_1);
}
v2f64
__lsx_vfcvth_d_s (v4f32 _1)
{
  return __builtin_lsx_vfcvth_d_s (_1);
}
v4f32
__lsx_vfcvtl_s_h (v8i16 _1)
{
  return __builtin_lsx_vfcvtl_s_h (_1);
}
v2f64
__lsx_vfcvtl_d_s (v4f32 _1)
{
  return __builtin_lsx_vfcvtl_d_s (_1);
}
v4i32
__lsx_vftint_w_s (v4f32 _1)
{
  return __builtin_lsx_vftint_w_s (_1);
}
v2i64
__lsx_vftint_l_d (v2f64 _1)
{
  return __builtin_lsx_vftint_l_d (_1);
}
v4u32
__lsx_vftint_wu_s (v4f32 _1)
{
  return __builtin_lsx_vftint_wu_s (_1);
}
v2u64
__lsx_vftint_lu_d (v2f64 _1)
{
  return __builtin_lsx_vftint_lu_d (_1);
}
v4i32
__lsx_vftintrz_w_s (v4f32 _1)
{
  return __builtin_lsx_vftintrz_w_s (_1);
}
v2i64
__lsx_vftintrz_l_d (v2f64 _1)
{
  return __builtin_lsx_vftintrz_l_d (_1);
}
v4u32
__lsx_vftintrz_wu_s (v4f32 _1)
{
  return __builtin_lsx_vftintrz_wu_s (_1);
}
v2u64
__lsx_vftintrz_lu_d (v2f64 _1)
{
  return __builtin_lsx_vftintrz_lu_d (_1);
}
v4f32
__lsx_vffint_s_w (v4i32 _1)
{
  return __builtin_lsx_vffint_s_w (_1);
}
v2f64
__lsx_vffint_d_l (v2i64 _1)
{
  return __builtin_lsx_vffint_d_l (_1);
}
v4f32
__lsx_vffint_s_wu (v4u32 _1)
{
  return __builtin_lsx_vffint_s_wu (_1);
}
v2f64
__lsx_vffint_d_lu (v2u64 _1)
{
  return __builtin_lsx_vffint_d_lu (_1);
}
v16u8
__lsx_vandn_v (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vandn_v (_1, _2);
}
v16i8
__lsx_vneg_b (v16i8 _1)
{
  return __builtin_lsx_vneg_b (_1);
}
v8i16
__lsx_vneg_h (v8i16 _1)
{
  return __builtin_lsx_vneg_h (_1);
}
v4i32
__lsx_vneg_w (v4i32 _1)
{
  return __builtin_lsx_vneg_w (_1);
}
v2i64
__lsx_vneg_d (v2i64 _1)
{
  return __builtin_lsx_vneg_d (_1);
}
v16i8
__lsx_vmuh_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmuh_b (_1, _2);
}
v8i16
__lsx_vmuh_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmuh_h (_1, _2);
}
v4i32
__lsx_vmuh_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmuh_w (_1, _2);
}
v2i64
__lsx_vmuh_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmuh_d (_1, _2);
}
v16u8
__lsx_vmuh_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vmuh_bu (_1, _2);
}
v8u16
__lsx_vmuh_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vmuh_hu (_1, _2);
}
v4u32
__lsx_vmuh_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vmuh_wu (_1, _2);
}
v2u64
__lsx_vmuh_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vmuh_du (_1, _2);
}
v8i16
__lsx_vsllwil_h_b (v16i8 _1)
{
  return __builtin_lsx_vsllwil_h_b (_1, 1);
}
v4i32
__lsx_vsllwil_w_h (v8i16 _1)
{
  return __builtin_lsx_vsllwil_w_h (_1, 1);
}
v2i64
__lsx_vsllwil_d_w (v4i32 _1)
{
  return __builtin_lsx_vsllwil_d_w (_1, 1);
}
v8u16
__lsx_vsllwil_hu_bu (v16u8 _1)
{
  return __builtin_lsx_vsllwil_hu_bu (_1, 1);
}
v4u32
__lsx_vsllwil_wu_hu (v8u16 _1)
{
  return __builtin_lsx_vsllwil_wu_hu (_1, 1);
}
v2u64
__lsx_vsllwil_du_wu (v4u32 _1)
{
  return __builtin_lsx_vsllwil_du_wu (_1, 1);
}
v16i8
__lsx_vsran_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsran_b_h (_1, _2);
}
v8i16
__lsx_vsran_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsran_h_w (_1, _2);
}
v4i32
__lsx_vsran_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsran_w_d (_1, _2);
}
v16i8
__lsx_vssran_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssran_b_h (_1, _2);
}
v8i16
__lsx_vssran_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssran_h_w (_1, _2);
}
v4i32
__lsx_vssran_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssran_w_d (_1, _2);
}
v16u8
__lsx_vssran_bu_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vssran_bu_h (_1, _2);
}
v8u16
__lsx_vssran_hu_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vssran_hu_w (_1, _2);
}
v4u32
__lsx_vssran_wu_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vssran_wu_d (_1, _2);
}
v16i8
__lsx_vsrarn_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrarn_b_h (_1, _2);
}
v8i16
__lsx_vsrarn_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrarn_h_w (_1, _2);
}
v4i32
__lsx_vsrarn_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrarn_w_d (_1, _2);
}
v16i8
__lsx_vssrarn_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrarn_b_h (_1, _2);
}
v8i16
__lsx_vssrarn_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrarn_h_w (_1, _2);
}
v4i32
__lsx_vssrarn_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrarn_w_d (_1, _2);
}
v16u8
__lsx_vssrarn_bu_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vssrarn_bu_h (_1, _2);
}
v8u16
__lsx_vssrarn_hu_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vssrarn_hu_w (_1, _2);
}
v4u32
__lsx_vssrarn_wu_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vssrarn_wu_d (_1, _2);
}
v16i8
__lsx_vsrln_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrln_b_h (_1, _2);
}
v8i16
__lsx_vsrln_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrln_h_w (_1, _2);
}
v4i32
__lsx_vsrln_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrln_w_d (_1, _2);
}
v16u8
__lsx_vssrln_bu_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vssrln_bu_h (_1, _2);
}
v8u16
__lsx_vssrln_hu_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vssrln_hu_w (_1, _2);
}
v4u32
__lsx_vssrln_wu_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vssrln_wu_d (_1, _2);
}
v16i8
__lsx_vsrlrn_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrlrn_b_h (_1, _2);
}
v8i16
__lsx_vsrlrn_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrlrn_h_w (_1, _2);
}
v4i32
__lsx_vsrlrn_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrlrn_w_d (_1, _2);
}
v16u8
__lsx_vssrlrn_bu_h (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vssrlrn_bu_h (_1, _2);
}
v8u16
__lsx_vssrlrn_hu_w (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vssrlrn_hu_w (_1, _2);
}
v4u32
__lsx_vssrlrn_wu_d (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vssrlrn_wu_d (_1, _2);
}
v16i8
__lsx_vfrstpi_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vfrstpi_b (_1, _2, 1);
}
v8i16
__lsx_vfrstpi_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vfrstpi_h (_1, _2, 1);
}
v16i8
__lsx_vfrstp_b (v16i8 _1, v16i8 _2, v16i8 _3)
{
  return __builtin_lsx_vfrstp_b (_1, _2, _3);
}
v8i16
__lsx_vfrstp_h (v8i16 _1, v8i16 _2, v8i16 _3)
{
  return __builtin_lsx_vfrstp_h (_1, _2, _3);
}
v2i64
__lsx_vshuf4i_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vshuf4i_d (_1, _2, 1);
}
v16i8
__lsx_vbsrl_v (v16i8 _1)
{
  return __builtin_lsx_vbsrl_v (_1, 1);
}
v16i8
__lsx_vbsll_v (v16i8 _1)
{
  return __builtin_lsx_vbsll_v (_1, 1);
}
v16i8
__lsx_vextrins_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vextrins_b (_1, _2, 1);
}
v8i16
__lsx_vextrins_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vextrins_h (_1, _2, 1);
}
v4i32
__lsx_vextrins_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vextrins_w (_1, _2, 1);
}
v2i64
__lsx_vextrins_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vextrins_d (_1, _2, 1);
}
v16i8
__lsx_vmskltz_b (v16i8 _1)
{
  return __builtin_lsx_vmskltz_b (_1);
}
v8i16
__lsx_vmskltz_h (v8i16 _1)
{
  return __builtin_lsx_vmskltz_h (_1);
}
v4i32
__lsx_vmskltz_w (v4i32 _1)
{
  return __builtin_lsx_vmskltz_w (_1);
}
v2i64
__lsx_vmskltz_d (v2i64 _1)
{
  return __builtin_lsx_vmskltz_d (_1);
}
v16i8
__lsx_vsigncov_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsigncov_b (_1, _2);
}
v8i16
__lsx_vsigncov_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsigncov_h (_1, _2);
}
v4i32
__lsx_vsigncov_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsigncov_w (_1, _2);
}
v2i64
__lsx_vsigncov_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsigncov_d (_1, _2);
}
v4f32
__lsx_vfmadd_s (v4f32 _1, v4f32 _2, v4f32 _3)
{
  return __builtin_lsx_vfmadd_s (_1, _2, _3);
}
v2f64
__lsx_vfmadd_d (v2f64 _1, v2f64 _2, v2f64 _3)
{
  return __builtin_lsx_vfmadd_d (_1, _2, _3);
}
v4f32
__lsx_vfmsub_s (v4f32 _1, v4f32 _2, v4f32 _3)
{
  return __builtin_lsx_vfmsub_s (_1, _2, _3);
}
v2f64
__lsx_vfmsub_d (v2f64 _1, v2f64 _2, v2f64 _3)
{
  return __builtin_lsx_vfmsub_d (_1, _2, _3);
}
v4f32
__lsx_vfnmadd_s (v4f32 _1, v4f32 _2, v4f32 _3)
{
  return __builtin_lsx_vfnmadd_s (_1, _2, _3);
}
v2f64
__lsx_vfnmadd_d (v2f64 _1, v2f64 _2, v2f64 _3)
{
  return __builtin_lsx_vfnmadd_d (_1, _2, _3);
}
v4f32
__lsx_vfnmsub_s (v4f32 _1, v4f32 _2, v4f32 _3)
{
  return __builtin_lsx_vfnmsub_s (_1, _2, _3);
}
v2f64
__lsx_vfnmsub_d (v2f64 _1, v2f64 _2, v2f64 _3)
{
  return __builtin_lsx_vfnmsub_d (_1, _2, _3);
}
v4i32
__lsx_vftintrne_w_s (v4f32 _1)
{
  return __builtin_lsx_vftintrne_w_s (_1);
}
v2i64
__lsx_vftintrne_l_d (v2f64 _1)
{
  return __builtin_lsx_vftintrne_l_d (_1);
}
v4i32
__lsx_vftintrp_w_s (v4f32 _1)
{
  return __builtin_lsx_vftintrp_w_s (_1);
}
v2i64
__lsx_vftintrp_l_d (v2f64 _1)
{
  return __builtin_lsx_vftintrp_l_d (_1);
}
v4i32
__lsx_vftintrm_w_s (v4f32 _1)
{
  return __builtin_lsx_vftintrm_w_s (_1);
}
v2i64
__lsx_vftintrm_l_d (v2f64 _1)
{
  return __builtin_lsx_vftintrm_l_d (_1);
}
v4i32
__lsx_vftint_w_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vftint_w_d (_1, _2);
}
v4f32
__lsx_vffint_s_l (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vffint_s_l (_1, _2);
}
v4i32
__lsx_vftintrz_w_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vftintrz_w_d (_1, _2);
}
v4i32
__lsx_vftintrp_w_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vftintrp_w_d (_1, _2);
}
v4i32
__lsx_vftintrm_w_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vftintrm_w_d (_1, _2);
}
v4i32
__lsx_vftintrne_w_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vftintrne_w_d (_1, _2);
}
v2i64
__lsx_vftintl_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintl_l_s (_1);
}
v2i64
__lsx_vftinth_l_s (v4f32 _1)
{
  return __builtin_lsx_vftinth_l_s (_1);
}
v2f64
__lsx_vffinth_d_w (v4i32 _1)
{
  return __builtin_lsx_vffinth_d_w (_1);
}
v2f64
__lsx_vffintl_d_w (v4i32 _1)
{
  return __builtin_lsx_vffintl_d_w (_1);
}
v2i64
__lsx_vftintrzl_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrzl_l_s (_1);
}
v2i64
__lsx_vftintrzh_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrzh_l_s (_1);
}
v2i64
__lsx_vftintrpl_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrpl_l_s (_1);
}
v2i64
__lsx_vftintrph_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrph_l_s (_1);
}
v2i64
__lsx_vftintrml_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrml_l_s (_1);
}
v2i64
__lsx_vftintrmh_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrmh_l_s (_1);
}
v2i64
__lsx_vftintrnel_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrnel_l_s (_1);
}
v2i64
__lsx_vftintrneh_l_s (v4f32 _1)
{
  return __builtin_lsx_vftintrneh_l_s (_1);
}
v4f32
__lsx_vfrintrne_s (v4f32 _1)
{
  return __builtin_lsx_vfrintrne_s (_1);
}
v2f64
__lsx_vfrintrne_d (v2f64 _1)
{
  return __builtin_lsx_vfrintrne_d (_1);
}
v4f32
__lsx_vfrintrz_s (v4f32 _1)
{
  return __builtin_lsx_vfrintrz_s (_1);
}
v2f64
__lsx_vfrintrz_d (v2f64 _1)
{
  return __builtin_lsx_vfrintrz_d (_1);
}
v4f32
__lsx_vfrintrp_s (v4f32 _1)
{
  return __builtin_lsx_vfrintrp_s (_1);
}
v2f64
__lsx_vfrintrp_d (v2f64 _1)
{
  return __builtin_lsx_vfrintrp_d (_1);
}
v4f32
__lsx_vfrintrm_s (v4f32 _1)
{
  return __builtin_lsx_vfrintrm_s (_1);
}
v2f64
__lsx_vfrintrm_d (v2f64 _1)
{
  return __builtin_lsx_vfrintrm_d (_1);
}
void
__lsx_vstelm_b (v16i8 _1, void *_2)
{
  return __builtin_lsx_vstelm_b (_1, _2, 1, 1);
}
void
__lsx_vstelm_h (v8i16 _1, void *_2)
{
  return __builtin_lsx_vstelm_h (_1, _2, 2, 1);
}
void
__lsx_vstelm_w (v4i32 _1, void *_2)
{
  return __builtin_lsx_vstelm_w (_1, _2, 4, 1);
}
void
__lsx_vstelm_d (v2i64 _1, void *_2)
{
  return __builtin_lsx_vstelm_d (_1, _2, 8, 1);
}
v2i64
__lsx_vaddwev_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vaddwev_d_w (_1, _2);
}
v4i32
__lsx_vaddwev_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vaddwev_w_h (_1, _2);
}
v8i16
__lsx_vaddwev_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vaddwev_h_b (_1, _2);
}
v2i64
__lsx_vaddwod_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vaddwod_d_w (_1, _2);
}
v4i32
__lsx_vaddwod_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vaddwod_w_h (_1, _2);
}
v8i16
__lsx_vaddwod_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vaddwod_h_b (_1, _2);
}
v2i64
__lsx_vaddwev_d_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vaddwev_d_wu (_1, _2);
}
v4i32
__lsx_vaddwev_w_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vaddwev_w_hu (_1, _2);
}
v8i16
__lsx_vaddwev_h_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vaddwev_h_bu (_1, _2);
}
v2i64
__lsx_vaddwod_d_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vaddwod_d_wu (_1, _2);
}
v4i32
__lsx_vaddwod_w_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vaddwod_w_hu (_1, _2);
}
v8i16
__lsx_vaddwod_h_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vaddwod_h_bu (_1, _2);
}
v2i64
__lsx_vaddwev_d_wu_w (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vaddwev_d_wu_w (_1, _2);
}
v4i32
__lsx_vaddwev_w_hu_h (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vaddwev_w_hu_h (_1, _2);
}
v8i16
__lsx_vaddwev_h_bu_b (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vaddwev_h_bu_b (_1, _2);
}
v2i64
__lsx_vaddwod_d_wu_w (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vaddwod_d_wu_w (_1, _2);
}
v4i32
__lsx_vaddwod_w_hu_h (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vaddwod_w_hu_h (_1, _2);
}
v8i16
__lsx_vaddwod_h_bu_b (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vaddwod_h_bu_b (_1, _2);
}
v2i64
__lsx_vsubwev_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsubwev_d_w (_1, _2);
}
v4i32
__lsx_vsubwev_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsubwev_w_h (_1, _2);
}
v8i16
__lsx_vsubwev_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsubwev_h_b (_1, _2);
}
v2i64
__lsx_vsubwod_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsubwod_d_w (_1, _2);
}
v4i32
__lsx_vsubwod_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsubwod_w_h (_1, _2);
}
v8i16
__lsx_vsubwod_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsubwod_h_b (_1, _2);
}
v2i64
__lsx_vsubwev_d_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vsubwev_d_wu (_1, _2);
}
v4i32
__lsx_vsubwev_w_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vsubwev_w_hu (_1, _2);
}
v8i16
__lsx_vsubwev_h_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vsubwev_h_bu (_1, _2);
}
v2i64
__lsx_vsubwod_d_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vsubwod_d_wu (_1, _2);
}
v4i32
__lsx_vsubwod_w_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vsubwod_w_hu (_1, _2);
}
v8i16
__lsx_vsubwod_h_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vsubwod_h_bu (_1, _2);
}
v2i64
__lsx_vaddwev_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vaddwev_q_d (_1, _2);
}
v2i64
__lsx_vaddwod_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vaddwod_q_d (_1, _2);
}
v2i64
__lsx_vaddwev_q_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vaddwev_q_du (_1, _2);
}
v2i64
__lsx_vaddwod_q_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vaddwod_q_du (_1, _2);
}
v2i64
__lsx_vsubwev_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsubwev_q_d (_1, _2);
}
v2i64
__lsx_vsubwod_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsubwod_q_d (_1, _2);
}
v2i64
__lsx_vsubwev_q_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vsubwev_q_du (_1, _2);
}
v2i64
__lsx_vsubwod_q_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vsubwod_q_du (_1, _2);
}
v2i64
__lsx_vaddwev_q_du_d (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vaddwev_q_du_d (_1, _2);
}
v2i64
__lsx_vaddwod_q_du_d (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vaddwod_q_du_d (_1, _2);
}
v2i64
__lsx_vmulwev_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmulwev_d_w (_1, _2);
}
v4i32
__lsx_vmulwev_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmulwev_w_h (_1, _2);
}
v8i16
__lsx_vmulwev_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmulwev_h_b (_1, _2);
}
v2i64
__lsx_vmulwod_d_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vmulwod_d_w (_1, _2);
}
v4i32
__lsx_vmulwod_w_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vmulwod_w_h (_1, _2);
}
v8i16
__lsx_vmulwod_h_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vmulwod_h_b (_1, _2);
}
v2i64
__lsx_vmulwev_d_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vmulwev_d_wu (_1, _2);
}
v4i32
__lsx_vmulwev_w_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vmulwev_w_hu (_1, _2);
}
v8i16
__lsx_vmulwev_h_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vmulwev_h_bu (_1, _2);
}
v2i64
__lsx_vmulwod_d_wu (v4u32 _1, v4u32 _2)
{
  return __builtin_lsx_vmulwod_d_wu (_1, _2);
}
v4i32
__lsx_vmulwod_w_hu (v8u16 _1, v8u16 _2)
{
  return __builtin_lsx_vmulwod_w_hu (_1, _2);
}
v8i16
__lsx_vmulwod_h_bu (v16u8 _1, v16u8 _2)
{
  return __builtin_lsx_vmulwod_h_bu (_1, _2);
}
v2i64
__lsx_vmulwev_d_wu_w (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vmulwev_d_wu_w (_1, _2);
}
v4i32
__lsx_vmulwev_w_hu_h (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vmulwev_w_hu_h (_1, _2);
}
v8i16
__lsx_vmulwev_h_bu_b (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vmulwev_h_bu_b (_1, _2);
}
v2i64
__lsx_vmulwod_d_wu_w (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vmulwod_d_wu_w (_1, _2);
}
v4i32
__lsx_vmulwod_w_hu_h (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vmulwod_w_hu_h (_1, _2);
}
v8i16
__lsx_vmulwod_h_bu_b (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vmulwod_h_bu_b (_1, _2);
}
v2i64
__lsx_vmulwev_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmulwev_q_d (_1, _2);
}
v2i64
__lsx_vmulwod_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vmulwod_q_d (_1, _2);
}
v2i64
__lsx_vmulwev_q_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vmulwev_q_du (_1, _2);
}
v2i64
__lsx_vmulwod_q_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vmulwod_q_du (_1, _2);
}
v2i64
__lsx_vmulwev_q_du_d (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vmulwev_q_du_d (_1, _2);
}
v2i64
__lsx_vmulwod_q_du_d (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vmulwod_q_du_d (_1, _2);
}
v2i64
__lsx_vhaddw_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vhaddw_q_d (_1, _2);
}
v2u64
__lsx_vhaddw_qu_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vhaddw_qu_du (_1, _2);
}
v2i64
__lsx_vhsubw_q_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vhsubw_q_d (_1, _2);
}
v2u64
__lsx_vhsubw_qu_du (v2u64 _1, v2u64 _2)
{
  return __builtin_lsx_vhsubw_qu_du (_1, _2);
}
v2i64
__lsx_vmaddwev_d_w (v2i64 _1, v4i32 _2, v4i32 _3)
{
  return __builtin_lsx_vmaddwev_d_w (_1, _2, _3);
}
v4i32
__lsx_vmaddwev_w_h (v4i32 _1, v8i16 _2, v8i16 _3)
{
  return __builtin_lsx_vmaddwev_w_h (_1, _2, _3);
}
v8i16
__lsx_vmaddwev_h_b (v8i16 _1, v16i8 _2, v16i8 _3)
{
  return __builtin_lsx_vmaddwev_h_b (_1, _2, _3);
}
v2u64
__lsx_vmaddwev_d_wu (v2u64 _1, v4u32 _2, v4u32 _3)
{
  return __builtin_lsx_vmaddwev_d_wu (_1, _2, _3);
}
v4u32
__lsx_vmaddwev_w_hu (v4u32 _1, v8u16 _2, v8u16 _3)
{
  return __builtin_lsx_vmaddwev_w_hu (_1, _2, _3);
}
v8u16
__lsx_vmaddwev_h_bu (v8u16 _1, v16u8 _2, v16u8 _3)
{
  return __builtin_lsx_vmaddwev_h_bu (_1, _2, _3);
}
v2i64
__lsx_vmaddwod_d_w (v2i64 _1, v4i32 _2, v4i32 _3)
{
  return __builtin_lsx_vmaddwod_d_w (_1, _2, _3);
}
v4i32
__lsx_vmaddwod_w_h (v4i32 _1, v8i16 _2, v8i16 _3)
{
  return __builtin_lsx_vmaddwod_w_h (_1, _2, _3);
}
v8i16
__lsx_vmaddwod_h_b (v8i16 _1, v16i8 _2, v16i8 _3)
{
  return __builtin_lsx_vmaddwod_h_b (_1, _2, _3);
}
v2u64
__lsx_vmaddwod_d_wu (v2u64 _1, v4u32 _2, v4u32 _3)
{
  return __builtin_lsx_vmaddwod_d_wu (_1, _2, _3);
}
v4u32
__lsx_vmaddwod_w_hu (v4u32 _1, v8u16 _2, v8u16 _3)
{
  return __builtin_lsx_vmaddwod_w_hu (_1, _2, _3);
}
v8u16
__lsx_vmaddwod_h_bu (v8u16 _1, v16u8 _2, v16u8 _3)
{
  return __builtin_lsx_vmaddwod_h_bu (_1, _2, _3);
}
v2i64
__lsx_vmaddwev_d_wu_w (v2i64 _1, v4u32 _2, v4i32 _3)
{
  return __builtin_lsx_vmaddwev_d_wu_w (_1, _2, _3);
}
v4i32
__lsx_vmaddwev_w_hu_h (v4i32 _1, v8u16 _2, v8i16 _3)
{
  return __builtin_lsx_vmaddwev_w_hu_h (_1, _2, _3);
}
v8i16
__lsx_vmaddwev_h_bu_b (v8i16 _1, v16u8 _2, v16i8 _3)
{
  return __builtin_lsx_vmaddwev_h_bu_b (_1, _2, _3);
}
v2i64
__lsx_vmaddwod_d_wu_w (v2i64 _1, v4u32 _2, v4i32 _3)
{
  return __builtin_lsx_vmaddwod_d_wu_w (_1, _2, _3);
}
v4i32
__lsx_vmaddwod_w_hu_h (v4i32 _1, v8u16 _2, v8i16 _3)
{
  return __builtin_lsx_vmaddwod_w_hu_h (_1, _2, _3);
}
v8i16
__lsx_vmaddwod_h_bu_b (v8i16 _1, v16u8 _2, v16i8 _3)
{
  return __builtin_lsx_vmaddwod_h_bu_b (_1, _2, _3);
}
v2i64
__lsx_vmaddwev_q_d (v2i64 _1, v2i64 _2, v2i64 _3)
{
  return __builtin_lsx_vmaddwev_q_d (_1, _2, _3);
}
v2i64
__lsx_vmaddwod_q_d (v2i64 _1, v2i64 _2, v2i64 _3)
{
  return __builtin_lsx_vmaddwod_q_d (_1, _2, _3);
}
v2u64
__lsx_vmaddwev_q_du (v2u64 _1, v2u64 _2, v2u64 _3)
{
  return __builtin_lsx_vmaddwev_q_du (_1, _2, _3);
}
v2u64
__lsx_vmaddwod_q_du (v2u64 _1, v2u64 _2, v2u64 _3)
{
  return __builtin_lsx_vmaddwod_q_du (_1, _2, _3);
}
v2i64
__lsx_vmaddwev_q_du_d (v2i64 _1, v2u64 _2, v2i64 _3)
{
  return __builtin_lsx_vmaddwev_q_du_d (_1, _2, _3);
}
v2i64
__lsx_vmaddwod_q_du_d (v2i64 _1, v2u64 _2, v2i64 _3)
{
  return __builtin_lsx_vmaddwod_q_du_d (_1, _2, _3);
}
v16i8
__lsx_vrotr_b (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vrotr_b (_1, _2);
}
v8i16
__lsx_vrotr_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vrotr_h (_1, _2);
}
v4i32
__lsx_vrotr_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vrotr_w (_1, _2);
}
v2i64
__lsx_vrotr_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vrotr_d (_1, _2);
}
v2i64
__lsx_vadd_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vadd_q (_1, _2);
}
v2i64
__lsx_vsub_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsub_q (_1, _2);
}
v16i8
__lsx_vldrepl_b (void *_1)
{
  return __builtin_lsx_vldrepl_b (_1, 1);
}
v8i16
__lsx_vldrepl_h (void *_1)
{
  return __builtin_lsx_vldrepl_h (_1, 2);
}
v4i32
__lsx_vldrepl_w (void *_1)
{
  return __builtin_lsx_vldrepl_w (_1, 4);
}
v2i64
__lsx_vldrepl_d (void *_1)
{
  return __builtin_lsx_vldrepl_d (_1, 8);
}
v16i8
__lsx_vmskgez_b (v16i8 _1)
{
  return __builtin_lsx_vmskgez_b (_1);
}
v16i8
__lsx_vmsknz_b (v16i8 _1)
{
  return __builtin_lsx_vmsknz_b (_1);
}
v8i16
__lsx_vexth_h_b (v16i8 _1)
{
  return __builtin_lsx_vexth_h_b (_1);
}
v4i32
__lsx_vexth_w_h (v8i16 _1)
{
  return __builtin_lsx_vexth_w_h (_1);
}
v2i64
__lsx_vexth_d_w (v4i32 _1)
{
  return __builtin_lsx_vexth_d_w (_1);
}
v2i64
__lsx_vexth_q_d (v2i64 _1)
{
  return __builtin_lsx_vexth_q_d (_1);
}
v8u16
__lsx_vexth_hu_bu (v16u8 _1)
{
  return __builtin_lsx_vexth_hu_bu (_1);
}
v4u32
__lsx_vexth_wu_hu (v8u16 _1)
{
  return __builtin_lsx_vexth_wu_hu (_1);
}
v2u64
__lsx_vexth_du_wu (v4u32 _1)
{
  return __builtin_lsx_vexth_du_wu (_1);
}
v2u64
__lsx_vexth_qu_du (v2u64 _1)
{
  return __builtin_lsx_vexth_qu_du (_1);
}
v16i8
__lsx_vrotri_b (v16i8 _1)
{
  return __builtin_lsx_vrotri_b (_1, 1);
}
v8i16
__lsx_vrotri_h (v8i16 _1)
{
  return __builtin_lsx_vrotri_h (_1, 1);
}
v4i32
__lsx_vrotri_w (v4i32 _1)
{
  return __builtin_lsx_vrotri_w (_1, 1);
}
v2i64
__lsx_vrotri_d (v2i64 _1)
{
  return __builtin_lsx_vrotri_d (_1, 1);
}
v2i64
__lsx_vextl_q_d (v2i64 _1)
{
  return __builtin_lsx_vextl_q_d (_1);
}
v16i8
__lsx_vsrlni_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrlni_b_h (_1, _2, 1);
}
v8i16
__lsx_vsrlni_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrlni_h_w (_1, _2, 1);
}
v4i32
__lsx_vsrlni_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrlni_w_d (_1, _2, 1);
}
v2i64
__lsx_vsrlni_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrlni_d_q (_1, _2, 1);
}
v16i8
__lsx_vsrlrni_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrlrni_b_h (_1, _2, 1);
}
v8i16
__lsx_vsrlrni_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrlrni_h_w (_1, _2, 1);
}
v4i32
__lsx_vsrlrni_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrlrni_w_d (_1, _2, 1);
}
v2i64
__lsx_vsrlrni_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrlrni_d_q (_1, _2, 1);
}
v16i8
__lsx_vssrlni_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrlni_b_h (_1, _2, 1);
}
v8i16
__lsx_vssrlni_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrlni_h_w (_1, _2, 1);
}
v4i32
__lsx_vssrlni_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrlni_w_d (_1, _2, 1);
}
v2i64
__lsx_vssrlni_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrlni_d_q (_1, _2, 1);
}
v16u8
__lsx_vssrlni_bu_h (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrlni_bu_h (_1, _2, 1);
}
v8u16
__lsx_vssrlni_hu_w (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrlni_hu_w (_1, _2, 1);
}
v4u32
__lsx_vssrlni_wu_d (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrlni_wu_d (_1, _2, 1);
}
v2u64
__lsx_vssrlni_du_q (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrlni_du_q (_1, _2, 1);
}
v16i8
__lsx_vssrlrni_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrlrni_b_h (_1, _2, 1);
}
v8i16
__lsx_vssrlrni_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrlrni_h_w (_1, _2, 1);
}
v4i32
__lsx_vssrlrni_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrlrni_w_d (_1, _2, 1);
}
v2i64
__lsx_vssrlrni_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrlrni_d_q (_1, _2, 1);
}
v16u8
__lsx_vssrlrni_bu_h (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrlrni_bu_h (_1, _2, 1);
}
v8u16
__lsx_vssrlrni_hu_w (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrlrni_hu_w (_1, _2, 1);
}
v4u32
__lsx_vssrlrni_wu_d (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrlrni_wu_d (_1, _2, 1);
}
v2u64
__lsx_vssrlrni_du_q (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrlrni_du_q (_1, _2, 1);
}
v16i8
__lsx_vsrani_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrani_b_h (_1, _2, 1);
}
v8i16
__lsx_vsrani_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrani_h_w (_1, _2, 1);
}
v4i32
__lsx_vsrani_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrani_w_d (_1, _2, 1);
}
v2i64
__lsx_vsrani_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrani_d_q (_1, _2, 1);
}
v16i8
__lsx_vsrarni_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vsrarni_b_h (_1, _2, 1);
}
v8i16
__lsx_vsrarni_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vsrarni_h_w (_1, _2, 1);
}
v4i32
__lsx_vsrarni_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vsrarni_w_d (_1, _2, 1);
}
v2i64
__lsx_vsrarni_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vsrarni_d_q (_1, _2, 1);
}
v16i8
__lsx_vssrani_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrani_b_h (_1, _2, 1);
}
v8i16
__lsx_vssrani_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrani_h_w (_1, _2, 1);
}
v4i32
__lsx_vssrani_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrani_w_d (_1, _2, 1);
}
v2i64
__lsx_vssrani_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrani_d_q (_1, _2, 1);
}
v16u8
__lsx_vssrani_bu_h (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrani_bu_h (_1, _2, 1);
}
v8u16
__lsx_vssrani_hu_w (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrani_hu_w (_1, _2, 1);
}
v4u32
__lsx_vssrani_wu_d (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrani_wu_d (_1, _2, 1);
}
v2u64
__lsx_vssrani_du_q (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrani_du_q (_1, _2, 1);
}
v16i8
__lsx_vssrarni_b_h (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrarni_b_h (_1, _2, 1);
}
v8i16
__lsx_vssrarni_h_w (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrarni_h_w (_1, _2, 1);
}
v4i32
__lsx_vssrarni_w_d (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrarni_w_d (_1, _2, 1);
}
v2i64
__lsx_vssrarni_d_q (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrarni_d_q (_1, _2, 1);
}
v16u8
__lsx_vssrarni_bu_h (v16u8 _1, v16i8 _2)
{
  return __builtin_lsx_vssrarni_bu_h (_1, _2, 1);
}
v8u16
__lsx_vssrarni_hu_w (v8u16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrarni_hu_w (_1, _2, 1);
}
v4u32
__lsx_vssrarni_wu_d (v4u32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrarni_wu_d (_1, _2, 1);
}
v2u64
__lsx_vssrarni_du_q (v2u64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrarni_du_q (_1, _2, 1);
}
v4i32
__lsx_vpermi_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vpermi_w (_1, _2, 1);
}
v16i8
__lsx_vld (void *_1)
{
  return __builtin_lsx_vld (_1, 1);
}
void
__lsx_vst (v16i8 _1, void *_2)
{
  return __builtin_lsx_vst (_1, _2, 1);
}
v16i8
__lsx_vssrlrn_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrlrn_b_h (_1, _2);
}
v8i16
__lsx_vssrlrn_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrlrn_h_w (_1, _2);
}
v4i32
__lsx_vssrlrn_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrlrn_w_d (_1, _2);
}
v16i8
__lsx_vssrln_b_h (v8i16 _1, v8i16 _2)
{
  return __builtin_lsx_vssrln_b_h (_1, _2);
}
v8i16
__lsx_vssrln_h_w (v4i32 _1, v4i32 _2)
{
  return __builtin_lsx_vssrln_h_w (_1, _2);
}
v4i32
__lsx_vssrln_w_d (v2i64 _1, v2i64 _2)
{
  return __builtin_lsx_vssrln_w_d (_1, _2);
}
v16i8
__lsx_vorn_v (v16i8 _1, v16i8 _2)
{
  return __builtin_lsx_vorn_v (_1, _2);
}
v2i64
__lsx_vldi ()
{
  return __builtin_lsx_vldi (1);
}
v16i8
__lsx_vshuf_b (v16i8 _1, v16i8 _2, v16i8 _3)
{
  return __builtin_lsx_vshuf_b (_1, _2, _3);
}
v16i8
__lsx_vldx (void *_1)
{
  return __builtin_lsx_vldx (_1, 1);
}
void
__lsx_vstx (v16i8 _1, void *_2)
{
  return __builtin_lsx_vstx (_1, _2, 1);
}
v2u64
__lsx_vextl_qu_du (v2u64 _1)
{
  return __builtin_lsx_vextl_qu_du (_1);
}
int
__lsx_bnz_b (v16u8 _1)
{
  return __builtin_lsx_bnz_b (_1);
}
int
__lsx_bnz_d (v2u64 _1)
{
  return __builtin_lsx_bnz_d (_1);
}
int
__lsx_bnz_h (v8u16 _1)
{
  return __builtin_lsx_bnz_h (_1);
}
int
__lsx_bnz_v (v16u8 _1)
{
  return __builtin_lsx_bnz_v (_1);
}
int
__lsx_bnz_w (v4u32 _1)
{
  return __builtin_lsx_bnz_w (_1);
}
int
__lsx_bz_b (v16u8 _1)
{
  return __builtin_lsx_bz_b (_1);
}
int
__lsx_bz_d (v2u64 _1)
{
  return __builtin_lsx_bz_d (_1);
}
int
__lsx_bz_h (v8u16 _1)
{
  return __builtin_lsx_bz_h (_1);
}
int
__lsx_bz_v (v16u8 _1)
{
  return __builtin_lsx_bz_v (_1);
}
int
__lsx_bz_w (v4u32 _1)
{
  return __builtin_lsx_bz_w (_1);
}
v2i64
__lsx_vfcmp_caf_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_caf_d (_1, _2);
}
v4i32
__lsx_vfcmp_caf_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_caf_s (_1, _2);
}
v2i64
__lsx_vfcmp_ceq_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_ceq_d (_1, _2);
}
v4i32
__lsx_vfcmp_ceq_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_ceq_s (_1, _2);
}
v2i64
__lsx_vfcmp_cle_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cle_d (_1, _2);
}
v4i32
__lsx_vfcmp_cle_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cle_s (_1, _2);
}
v2i64
__lsx_vfcmp_clt_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_clt_d (_1, _2);
}
v4i32
__lsx_vfcmp_clt_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_clt_s (_1, _2);
}
v2i64
__lsx_vfcmp_cne_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cne_d (_1, _2);
}
v4i32
__lsx_vfcmp_cne_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cne_s (_1, _2);
}
v2i64
__lsx_vfcmp_cor_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cor_d (_1, _2);
}
v4i32
__lsx_vfcmp_cor_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cor_s (_1, _2);
}
v2i64
__lsx_vfcmp_cueq_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cueq_d (_1, _2);
}
v4i32
__lsx_vfcmp_cueq_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cueq_s (_1, _2);
}
v2i64
__lsx_vfcmp_cule_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cule_d (_1, _2);
}
v4i32
__lsx_vfcmp_cule_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cule_s (_1, _2);
}
v2i64
__lsx_vfcmp_cult_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cult_d (_1, _2);
}
v4i32
__lsx_vfcmp_cult_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cult_s (_1, _2);
}
v2i64
__lsx_vfcmp_cun_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cun_d (_1, _2);
}
v2i64
__lsx_vfcmp_cune_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_cune_d (_1, _2);
}
v4i32
__lsx_vfcmp_cune_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cune_s (_1, _2);
}
v4i32
__lsx_vfcmp_cun_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_cun_s (_1, _2);
}
v2i64
__lsx_vfcmp_saf_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_saf_d (_1, _2);
}
v4i32
__lsx_vfcmp_saf_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_saf_s (_1, _2);
}
v2i64
__lsx_vfcmp_seq_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_seq_d (_1, _2);
}
v4i32
__lsx_vfcmp_seq_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_seq_s (_1, _2);
}
v2i64
__lsx_vfcmp_sle_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sle_d (_1, _2);
}
v4i32
__lsx_vfcmp_sle_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sle_s (_1, _2);
}
v2i64
__lsx_vfcmp_slt_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_slt_d (_1, _2);
}
v4i32
__lsx_vfcmp_slt_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_slt_s (_1, _2);
}
v2i64
__lsx_vfcmp_sne_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sne_d (_1, _2);
}
v4i32
__lsx_vfcmp_sne_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sne_s (_1, _2);
}
v2i64
__lsx_vfcmp_sor_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sor_d (_1, _2);
}
v4i32
__lsx_vfcmp_sor_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sor_s (_1, _2);
}
v2i64
__lsx_vfcmp_sueq_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sueq_d (_1, _2);
}
v4i32
__lsx_vfcmp_sueq_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sueq_s (_1, _2);
}
v2i64
__lsx_vfcmp_sule_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sule_d (_1, _2);
}
v4i32
__lsx_vfcmp_sule_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sule_s (_1, _2);
}
v2i64
__lsx_vfcmp_sult_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sult_d (_1, _2);
}
v4i32
__lsx_vfcmp_sult_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sult_s (_1, _2);
}
v2i64
__lsx_vfcmp_sun_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sun_d (_1, _2);
}
v2i64
__lsx_vfcmp_sune_d (v2f64 _1, v2f64 _2)
{
  return __builtin_lsx_vfcmp_sune_d (_1, _2);
}
v4i32
__lsx_vfcmp_sune_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sune_s (_1, _2);
}
v4i32
__lsx_vfcmp_sun_s (v4f32 _1, v4f32 _2)
{
  return __builtin_lsx_vfcmp_sun_s (_1, _2);
}
v16i8
__lsx_vrepli_b ()
{
  return __builtin_lsx_vrepli_b (1);
}
v2i64
__lsx_vrepli_d ()
{
  return __builtin_lsx_vrepli_d (1);
}
v8i16
__lsx_vrepli_h ()
{
  return __builtin_lsx_vrepli_h (1);
}
v4i32
__lsx_vrepli_w ()
{
  return __builtin_lsx_vrepli_w (1);
}
