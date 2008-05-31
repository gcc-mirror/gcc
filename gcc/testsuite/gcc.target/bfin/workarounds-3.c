/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "-mcpu=bf537-none" } */

#ifndef __ADSPBF537__
#error "__ADSPBF537__ is not defined"
#endif

#ifdef __SILICON_REVISION__
#error "__SILICON_REVISION__ is defined"
#endif

#ifdef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is defined"
#endif

#ifdef __WORKAROUND_RETS
#error "__WORKAROUND_RETS is defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_LOADS
#error "__WORKAROUND_SPECULATIVE_LOADS is defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_SYNCS
#error "__WORKAROUND_SPECULATIVE_SYNCS is defined"
#endif
