/* { dg-do compile }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0 -march=armv8-a -fdump-ipa-targetclone1-details" } */

#include "fmv_priority.in"

// Checks that the versions are in the correct order
// Each of these lines checks 3 consecutive versions in the list with one overlap
/* { dg-final { scan-ipa-dump-times "Version order for fn/\[0-9\]+:\\nfn\.default/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\.default/\[0-9\]+\\nfn\._Mrng/\[0-9\]+\\nfn\._Mflagm/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mflagm/\[0-9\]+\\nfn\._Mflagm2/\[0-9\]+\\nfn\._Mlse/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mlse/\[0-9\]+\\nfn\._Mfp/\[0-9\]+\\nfn\._Msimd/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msimd/\[0-9\]+\\nfn\._Mdotprod/\[0-9\]+\\nfn\._Msm4/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msm4/\[0-9\]+\\nfn\._MrdmaMrdm/\[0-9\]+\\nfn\._Mcrc/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mcrc/\[0-9\]+\\nfn\._Msha2/\[0-9\]+\\nfn\._Msha3/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msha3/\[0-9\]+\\nfn\._Maes/\[0-9\]+\\nfn\._Mfp16/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mfp16/\[0-9\]+\\nfn\._Mfp16fml/\[0-9\]+\\nfn\._Mjscvt/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mjscvt/\[0-9\]+\\nfn\._Mfcma/\[0-9\]+\\nfn\._Mrcpc/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mrcpc/\[0-9\]+\\nfn\._Mrcpc2/\[0-9\]+\\nfn\._Mrcpc3/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mrcpc3/\[0-9\]+\\nfn\._Mfrintts/\[0-9\]+\\nfn\._Mi8mm/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mi8mm/\[0-9\]+\\nfn\._Mbf16/\[0-9\]+\\nfn\._Msve/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msve/\[0-9\]+\\nfn\._Mf32mm/\[0-9\]+\\nfn\._Mf64mm/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mf64mm/\[0-9\]+\\nfn\._Msve2/\[0-9\]+\\nfn\._Msve2_aes/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msve2_aes/\[0-9\]+\\nfn\._Msve2_bitperm/\[0-9\]+\\nfn\._Msve2_sha3/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msve2_sha3/\[0-9\]+\\nfn\._Msve2_sm4/\[0-9\]+\\nfn\._Msve2Msme/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msve2Msme/\[0-9\]+\\nfn\._Msb/\[0-9\]+\\nfn\._Mwfxt/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mwfxt/\[0-9\]+\\nfn\._Msve2Msme_f64f64/\[0-9\]+\\nfn\._Msve2Msme_i16i64/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Msve2Msme_i16i64/\[0-9\]+\\nfn\._Msve2Msme2/\[0-9\]+\\nfn\._Mmops/\[0-9\]+\\n" 1 "targetclone1" } } */
/* { dg-final { scan-ipa-dump-times "fn\._Mmops/\[0-9\]+\\nfn\._Mcssc/\[0-9\]+\\n" 1 "targetclone1" } } */
