/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "-mcpu=bf527" } */

#ifndef __ADSPBF527__
#error "__ADSPBF527__ is not defined"
#endif

#ifndef __ADSPBF52x__
#error "__ADSPBF52x__ is not defined"
#endif

#if __SILICON_REVISION__ != 0x0002
#error "__SILICON_REVISION__ is not 0x0002"
#endif

#ifndef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is not defined"
#endif

#if __SILICON_REVISION__ <= 0x0001
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
