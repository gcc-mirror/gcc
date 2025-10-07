/* { dg-do run { target { aarch64_asm_sme2_ok } } }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0 -march=armv8-a" } */

#include <sys/auxv.h>
#include "../../../common/config/aarch64/cpuinfo.h"

/* This test has a FMV function set with one version per feature we support.
   Each version is turned on incrementally and the generated resolver is
   checked to show the correct version is chosen.  */

/* The resolver does actually take arguments, but ignores them and uses
   __aarch64_cpu_features global instead to establish what features are
   present.  */
int (*(resolver)(void)) (int) asm("fn.resolver");

extern struct {
  unsigned long long features;
} aarch64_cpu_features asm("__aarch64_cpu_features");

#include "fmv_priority.in"

#define setCPUFeature(F) aarch64_cpu_features.features |= 1UL << F

int main () {
  aarch64_cpu_features.features = 0;

  /* Initialize the CPU features, so the resolver doesn't try fetch it.  */
  setCPUFeature(FEAT_INIT);

  /* Go through features in order and assure the priorities are correct.
     By checking the correct versions are resolved.  */

  /* Some are missing as they are defined in the ACLE but are not yet
     implemented.  */
  if (resolver() != &fn_default) return 1;

  setCPUFeature(FEAT_RNG);
  if (resolver() != &fn_rng) return 1;

  setCPUFeature(FEAT_FLAGM);
 if (resolver() != &fn_flagm) return 1;

  setCPUFeature(FEAT_FLAGM2);
  if (resolver() != &fn_flagm2) return 1;

  setCPUFeature (FEAT_LSE);
  if (resolver () != &fn_lse) return 1;

  setCPUFeature (FEAT_FP);
  if (resolver () != &fn_fp) return 1;

  setCPUFeature (FEAT_SIMD);
  if (resolver () != &fn_simd) return 1;

  setCPUFeature (FEAT_DOTPROD);
  if (resolver () != &fn_dotprod) return 1;

  setCPUFeature (FEAT_SM4);
  if(resolver() != &fn_sm4) return 1;

  setCPUFeature (FEAT_RDM);
  if(resolver() != &fn_rdm) return 1;

  setCPUFeature (FEAT_CRC);
  if (resolver () != &fn_crc) return 1;

  setCPUFeature (FEAT_SHA2);
  if (resolver () != &fn_sha2) return 1;

  setCPUFeature (FEAT_SHA3);
  if (resolver () != &fn_sha3) return 1;

  setCPUFeature(FEAT_PMULL);
  if(resolver() != &fn_aes) return 1;

  setCPUFeature (FEAT_FP16);
  if (resolver () != &fn_fp16) return 1;

  setCPUFeature (FEAT_FP16FML);
  if(resolver() != &fn_fp16fml) return 1;

  setCPUFeature (FEAT_DIT);
  // if(resolver() != &fn_dit) return 1;
  // 
  setCPUFeature (FEAT_DPB);
  // if(resolver() != &fn_dpb) return 1;
  // 
  setCPUFeature (FEAT_DPB2);
  // if(resolver() != &fn_dpb2) return 1;
  // 
  setCPUFeature (FEAT_JSCVT);
  if (resolver () != &fn_jscvt) return 1;

  setCPUFeature (FEAT_FCMA);
  if (resolver () != &fn_fcma) return 1;

  setCPUFeature (FEAT_RCPC);
  if (resolver () != &fn_rcpc) return 1;

  setCPUFeature (FEAT_RCPC2);
  if (resolver () != &fn_rcpc2) return 1;

  setCPUFeature (FEAT_RCPC3);
  // if(resolver() != &fn_rcpc3) return 1;
  // 
  setCPUFeature (FEAT_FRINTTS);
  if (resolver () != &fn_frintts) return 1;

  setCPUFeature (FEAT_I8MM);
  if (resolver () != &fn_i8mm) return 1;

  setCPUFeature (FEAT_BF16);
  if (resolver () != &fn_bf16) return 1;

  setCPUFeature (FEAT_SVE);
  if (resolver () != &fn_sve) return 1;

  setCPUFeature (FEAT_SVE_F32MM);
  if(resolver() != &fn_f32mm) return 1;

  setCPUFeature (FEAT_SVE_F64MM);
  if(resolver() != &fn_f64mm) return 1;

  setCPUFeature (FEAT_SVE2);
  if (resolver () != &fn_sve2) return 1;

  setCPUFeature(FEAT_SVE_PMULL128);
  if(resolver() != &fn_sve2_aes) return 1;

  setCPUFeature (FEAT_SVE_BITPERM);
  if (resolver () != &fn_sve2_bitperm) return 1;

  setCPUFeature (FEAT_SVE_SHA3);
  if (resolver () != &fn_sve2_sha3) return 1;

  setCPUFeature (FEAT_SVE_SM4);
  if (resolver () != &fn_sve2_sm4) return 1;

  setCPUFeature (FEAT_SME);
  if (resolver () != &fn_sve2_sme) return 1;

  setCPUFeature(FEAT_MEMTAG2);
  // if(resolver() != &fn_memtag) return 1;

  setCPUFeature (FEAT_SB);
  if (resolver () != &fn_sb) return 1;

  setCPUFeature(FEAT_SSBS2);
  // if(resolver() != &fn_ssbs) return 1;
 
  setCPUFeature(FEAT_BTI);
  // if(resolver() != &fn_bti) return 1;

  setCPUFeature (FEAT_WFXT);
  if (resolver () != &fn_wfxt) return 1;

  setCPUFeature (FEAT_SME_F64);
  if (resolver () != &fn_sve2_sme_f64f64) return 1;

  setCPUFeature (FEAT_SME_I64);
  if (resolver () != &fn_sve2_sme_i16i64) return 1;

  setCPUFeature (FEAT_SME2);
  if (resolver () != &fn_sve2_sme2) return 1;

  setCPUFeature (FEAT_MOPS);
  if (resolver () != &fn_mops) return 1;

  setCPUFeature (FEAT_CSSC);
  if (resolver () != &fn_cssc) return 1;

  return 0;
}

