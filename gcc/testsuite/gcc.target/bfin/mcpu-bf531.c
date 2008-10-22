/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "-mcpu=bf531" } */

#ifndef __ADSPBF531__
#error "__ADSPBF531__ is not defined"
#endif

#if __SILICON_REVISION__ != 0x0006
#error "__SILICON_REVISION__ is not 0x0006"
#endif

#ifndef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is not defined"
#endif

#if __SILICON_REVISION__ <= 0x0005
#ifndef __WORKAROUND_RETS
#error "__WORKAROUND_RETS is not defined"
#endif
#else
#ifdef __WORKAROUND_RETS
#error "__WORKAROUND_RETS is defined"
#endif
#endif

#ifndef __WORKAROUND_SPECULATIVE_LOADS
#error "__WORKAROUND_SPECULATIVE_LOADS is not defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_SYNCS
#error "__WORKAROUND_SPECULATIVE_SYNCS is defined"
#endif
