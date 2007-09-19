/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "" } */

#ifndef __ADSPBF532__
#error "__ADSPBF532__ is not defined"
#endif

#if __SILICON_REVISION__ != 0x0005
#error "__SILICON_REVISION__ is not 0x0005"
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
