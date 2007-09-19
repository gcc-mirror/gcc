/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "-mno-specld-anomaly -mcpu=bf537" } */

#ifndef __ADSPBF537__
#error "__ADSPBF537__ is not defined"
#endif

#if __SILICON_REVISION__ != 0x0003
#error "__SILICON_REVISION__ is not 0x0003"
#endif

#ifdef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_LOADS
#error "__WORKAROUND_SPECULATIVE_LOADS is defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_SYNCS
#error "__WORKAROUND_SPECULATIVE_SYNCS is defined"
#endif
