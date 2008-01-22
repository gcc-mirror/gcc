/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "-mcpu=bf547" } */

#ifndef __ADSPBF547__
#error "__ADSPBF547__ is not defined"
#endif

#ifndef __ADSPBF54x__
#error "__ADSPBF54x__ is not defined"
#endif

#if __SILICON_REVISION__ != 0x0000
#error "__SILICON_REVISION__ is not 0x0000"
#endif

#ifndef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is not defined"
#endif

#ifndef __WORKAROUND_SPECULATIVE_LOADS
#error "__WORKAROUND_SPECULATIVE_LOADS is not defined"
#endif

#ifdef __WORKAROUND_SPECULATIVE_SYNCS
#error "__WORKAROUND_SPECULATIVE_SYNCS is defined"
#endif
