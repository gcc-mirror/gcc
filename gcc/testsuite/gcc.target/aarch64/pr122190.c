/* { dg-do compile }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0 -march=armv8-a -fdump-ipa-targetclone1-details" } */

int fn (int a) {return 1;}
int fn[[gnu::target_version("sve")]] (int a) {return 2;}
int fn[[gnu::target_version("simd+dotprod")]] (int a) {return 3;}
int fn[[gnu::target_version("sve+fp")]] (int a) {return 2;}

/* { dg-final { scan-ipa-dump-times "Version order for fn/\[0-9\]+:\\nfn\.default/\[0-9\]+\\nfn\._MsimdMdotprod/\[0-9\]+\\nfn\._Msve/\[0-9\]+\\nfn\._MfpMsve/\[0-9\]+\\n" 1 "targetclone1" } } */
