if (TARGET_64BIT)
  ADD_TARGET_INFO ("target_arch", "x86_64");
else
  ADD_TARGET_INFO ("target_arch", "x86");

// features officially "stabilised" in rustc
if (TARGET_MMX)
  ADD_TARGET_INFO ("target_feature", "mmx");
if (TARGET_SSE)
  ADD_TARGET_INFO ("target_feature", "sse");
if (TARGET_SSE2)
  ADD_TARGET_INFO ("target_feature", "sse2");
if (TARGET_SSE3)
  ADD_TARGET_INFO ("target_feature", "sse3");
if (TARGET_SSSE3)
  ADD_TARGET_INFO ("target_feature", "ssse3");
if (TARGET_SSE4_1)
  ADD_TARGET_INFO ("target_feature", "sse4.1");
if (TARGET_SSE4_2)
  ADD_TARGET_INFO ("target_feature", "sse4.2");
if (TARGET_AES)
  ADD_TARGET_INFO ("target_feature", "aes");
if (TARGET_SHA)
  ADD_TARGET_INFO ("target_feature", "sha");
if (TARGET_AVX)
  ADD_TARGET_INFO ("target_feature", "avx");
if (TARGET_AVX2)
  ADD_TARGET_INFO ("target_feature", "avx2");
if (TARGET_AVX512F)
  ADD_TARGET_INFO ("target_feature", "avx512f");
if (TARGET_AVX512CD)
  ADD_TARGET_INFO ("target_feature", "avx512cd");
if (TARGET_AVX512DQ)
  ADD_TARGET_INFO ("target_feature", "avx512dq");
if (TARGET_AVX512BW)
  ADD_TARGET_INFO ("target_feature", "avx512bw");
if (TARGET_AVX512VL)
  ADD_TARGET_INFO ("target_feature", "avx512vl");
if (TARGET_AVX512VBMI)
  ADD_TARGET_INFO ("target_feature", "avx512vbmi");
if (TARGET_AVX512IFMA)
  ADD_TARGET_INFO ("target_feature", "avx512ifma");
if (TARGET_AVX512VPOPCNTDQ)
  ADD_TARGET_INFO ("target_feature", "avx512vpopcntdq");
if (TARGET_FMA)
  ADD_TARGET_INFO ("target_feature", "fma");
if (TARGET_RTM)
  ADD_TARGET_INFO ("target_feature", "rtm");
if (TARGET_SSE4A)
  ADD_TARGET_INFO ("target_feature", "sse4a");
if (TARGET_BMI)
  {
    ADD_TARGET_INFO ("target_feature", "bmi1");
    ADD_TARGET_INFO ("target_feature", "bmi");
  }
if (TARGET_BMI2)
  ADD_TARGET_INFO ("target_feature", "bmi2");
if (TARGET_LZCNT)
  ADD_TARGET_INFO ("target_feature", "lzcnt");
if (TARGET_TBM)
  ADD_TARGET_INFO ("target_feature", "tbm");
if (TARGET_POPCNT)
  ADD_TARGET_INFO ("target_feature", "popcnt");
if (TARGET_RDRND)
  {
    ADD_TARGET_INFO ("target_feature", "rdrand");
    ADD_TARGET_INFO ("target_feature", "rdrnd");
  }
if (TARGET_F16C)
  ADD_TARGET_INFO ("target_feature", "f16c");
if (TARGET_RDSEED)
  ADD_TARGET_INFO ("target_feature", "rdseed");
if (TARGET_ADX)
  ADD_TARGET_INFO ("target_feature", "adx");
if (TARGET_FXSR)
  ADD_TARGET_INFO ("target_feature", "fxsr");
if (TARGET_XSAVE)
  ADD_TARGET_INFO ("target_feature", "xsave");
if (TARGET_XSAVEOPT)
  ADD_TARGET_INFO ("target_feature", "xsaveopt");
if (TARGET_XSAVEC)
  ADD_TARGET_INFO ("target_feature", "xsavec");
if (TARGET_XSAVES)
  ADD_TARGET_INFO ("target_feature", "xsaves");
if (TARGET_VPCLMULQDQ)
  {
    ADD_TARGET_INFO ("target_feature", "pclmulqdq");
    ADD_TARGET_INFO ("target_feature", "vpclmulqdq");
  }
if (TARGET_CMPXCHG16B)
  ADD_TARGET_INFO ("target_feature", "cmpxchg16b");
if (TARGET_MOVBE)
  ADD_TARGET_INFO ("target_feature", "movbe");
