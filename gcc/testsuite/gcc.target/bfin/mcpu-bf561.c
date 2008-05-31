/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "-mcpu=bf561" } */

#ifndef __ADSPBF561__
#error "__ADSPBF561__ is not defined"
#endif

#if __SILICON_REVISION__ != 0x0005
#error "__SILICON_REVISION__ is not 0x0005"
#endif

#ifndef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is not defined"
#endif

#ifndef __WORKAROUND_RETS
#error "__WORKAROUND_RETS is not defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_LOADS
#error "__WORKAROUND_SPECULATIVE_LOADS is defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_SYNCS
#error "__WORKAROUND_SPECULATIVE_SYNCS is defined"
#endif
