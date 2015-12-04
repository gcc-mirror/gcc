/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do assemble { target { lp64 } } } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-save-temps -mdebug -m64 -march=z10 -mtune=z13 -mstack-size=2048 -mstack-guard=16 -mbranch-cost=1 -mwarn-framesize=512 -mno-hard-dfp -mbackchain -msoft-float -mno-vx -mno-htm -mno-packed-stack -msmall-exec -mno-zvector -mmvcle -mzarch -mno-warn-dynamicstack" } */

/**
 **
 ** Start
 **
 **/

void fn_default_start (void) { }
/* { dg-final { scan-assembler "fn:fn_default_start ar6" } } */
/* { dg-final { scan-assembler "fn:fn_default_start tu9" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_default_start sg16" } } */
/* { dg-final { scan-assembler "fn:fn_default_start bc1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start wf512" } } */
/* { dg-final { scan-assembler "fn:fn_default_start hd0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ba1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start hf0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start vx0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ht0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ps0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start se1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start zv0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start mv1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start wd0" } } */

/**
 **
 ** Pragma
 **
 **/

#pragma GCC target ("no-zvector")
void fn_pragma_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0 zv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 tu9" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 sg16" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 bc1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 wf512" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 hd0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ba1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 hf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 vx0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ht0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ps0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 se1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 mv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 wd0" } } */
#pragma GCC reset_options

void fn_pragma_0_default (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default tu9" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default sg16" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default bc1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default wf512" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default hd0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ba1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default hf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default vx0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ht0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ps0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default se1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default zv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default mv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default wd0" } } */

#pragma GCC target ("zvector")
void fn_pragma_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 tu9" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 sg16" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 bc1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 wf512" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 hd0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ba1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 hf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 vx0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ht0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ps0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 se1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 mv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 wd0" } } */
#pragma GCC reset_options

void fn_pragma_1_default (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default tu9" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default sg16" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default bc1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default wf512" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default hd0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ba1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default hf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default vx0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ht0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ps0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default se1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default zv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default mv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default wd0" } } */

#pragma GCC target ("zvector")
#pragma GCC target ("no-zvector")
void fn_pragma_1_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 zv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 tu9" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 sg16" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 bc1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 wf512" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 hd0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ba1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 hf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 vx0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ht0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ps0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 se1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 mv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 wd0" } } */
#pragma GCC reset_options

#pragma GCC target ("no-zvector")
#pragma GCC target ("zvector")
void fn_pragma_0_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 tu9" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 sg16" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 bc1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 wf512" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 hd0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ba1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 hf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 vx0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ht0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ps0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 se1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 mv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 wd0" } } */
#pragma GCC reset_options

/**
 **
 ** End
 **
 **/

void fn_default_end (void) { }
/* { dg-final { scan-assembler "fn:fn_default_end ar6" } } */
/* { dg-final { scan-assembler "fn:fn_default_end tu9" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ss2048" } } */
/* { dg-final { scan-assembler "fn:fn_default_end sg16" } } */
/* { dg-final { scan-assembler "fn:fn_default_end bc1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end wf512" } } */
/* { dg-final { scan-assembler "fn:fn_default_end hd0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ba1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end hf0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end vx0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ht0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ps0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end se1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end zv0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end mv1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end wd0" } } */
