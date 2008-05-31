/* Test for -mcpu=.  */
/* { dg-do preprocess } */
/* { dg-bfin-options "" } */

#ifdef __ADSPBF522__
#error "__ADSPBF522__ is defined"
#endif
#ifdef __ADSPBF523__
#error "__ADSPBF523__ is defined"
#endif
#ifdef __ADSPBF524__
#error "__ADSPBF524__ is defined"
#endif
#ifdef __ADSPBF525__
#error "__ADSPBF525__ is defined"
#endif
#ifdef __ADSPBF526__
#error "__ADSPBF526__ is defined"
#endif
#ifdef __ADSPBF527__
#error "__ADSPBF527__ is defined"
#endif


#ifdef __ADSPBF531__
#error "__ADSPBF531__ is defined"
#endif
#ifdef __ADSPBF532__
#error "__ADSPBF532__ is defined"
#endif
#ifdef __ADSPBF533__
#error "__ADSPBF533__ is defined"
#endif
#ifdef __ADSPBF534__
#error "__ADSPBF534__ is defined"
#endif
#ifdef __ADSPBF536__
#error "__ADSPBF536__ is defined"
#endif
#ifdef __ADSPBF537__
#error "__ADSPBF537__ is defined"
#endif
#ifdef __ADSPBF538__
#error "__ADSPBF538__ is defined"
#endif
#ifdef __ADSPBF539__
#error "__ADSPBF539__ is defined"
#endif

#ifdef __ADSPBF542__
#error "__ADSPBF542__ is defined"
#endif
#ifdef __ADSPBF544__
#error "__ADSPBF544__ is defined"
#endif
#ifdef __ADSPBF547__
#error "__ADSPBF547__ is defined"
#endif
#ifdef __ADSPBF548__
#error "__ADSPBF548__ is defined"
#endif
#ifdef __ADSPBF549__
#error "__ADSPBF548__ is defined"
#endif

#ifdef __ADSPBF561__
#error "__ADSPBF561__ is defined"
#endif


#ifndef __SILICON_REVISION__
#error "__SILICON_REVISION__ is not defined"
#else
#if __SILICON_REVISION__ != 0xffff
#error "__SILICON_REVISION__ is not 0xFFFF"
#endif
#endif

#ifndef __WORKAROUNDS_ENABLED
#error "__WORKAROUNDS_ENABLED is not defined"
#endif

#ifndef __WORKAROUND_RETS
#error "__WORKAROUND_RETS is not defined"
#endif

#ifndef __WORKAROUND_SPECULATIVE_LOADS
#error "__WORKAROUND_SPECULATIVE_LOADS is not defined"
#endif

#ifndef __WORKAROUND_SPECULATIVE_SYNCS
#error "__WORKAROUND_SPECULATIVE_SYNCS is not defined"
#endif
