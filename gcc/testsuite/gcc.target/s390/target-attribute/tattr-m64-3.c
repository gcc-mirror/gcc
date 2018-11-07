/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do assemble { target { lp64 } } } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-save-temps -mdebug -m64 -march=z13 -mtune=z10 -mstack-size=4096 -mstack-guard=0 -mbranch-cost=2 -mwarn-framesize=0 -mhard-dfp -mno-backchain -mhard-float -mvx -mhtm -mpacked-stack -mno-small-exec -mzvector -mno-mvcle -mzarch -mwarn-dynamicstack" } */

/**
 **
 ** Start
 **
 **/

void fn_default_start (void) { }
/* { dg-final { scan-assembler "fn:fn_default_start ar7" } } */
/* { dg-final { scan-assembler "fn:fn_default_start tu4" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_default_start sg0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start bc2" } } */
/* { dg-final { scan-assembler "fn:fn_default_start wf0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start hd1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ba0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start hf1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start vx1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ht1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start ps1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start se0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start zv1" } } */
/* { dg-final { scan-assembler "fn:fn_default_start mv0" } } */
/* { dg-final { scan-assembler "fn:fn_default_start wd1" } } */

/**
 **
 ** Attribute
 **
 **/

__attribute__ ((target ("tune=z13")))
void fn_att_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_att_1 tu7" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 se0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1 wd1" } } */

void fn_att_1_default (void) { }

__attribute__ ((target ("tune=z10")))
void fn_att_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_att_0 tu4" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 se0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0 wd1" } } */

void fn_att_0_default (void) { }

__attribute__ ((target ("tune=z10,tune=z13")))
void fn_att_0_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_att_0_1 tu7" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 se0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_att_0_1 wd1" } } */

__attribute__ ((target ("tune=z13,tune=z10")))
void fn_att_1_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_att_1_0 tu4" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 se0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_att_1_0 wd1" } } */

/**
 **
 ** Pragma
 **
 **/

#pragma GCC target ("tune=z13")
void fn_pragma_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1 tu7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1 wd1" } } */
#pragma GCC reset_options

void fn_pragma_1_default (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default tu4" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_default wd1" } } */

#pragma GCC target ("tune=z10")
void fn_pragma_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0 tu4" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0 wd1" } } */
#pragma GCC reset_options

void fn_pragma_0_default (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default tu4" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_default wd1" } } */

#pragma GCC target ("tune=z10")
#pragma GCC target ("tune=z13")
void fn_pragma_0_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 tu7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_1 wd1" } } */
#pragma GCC reset_options

#pragma GCC target ("tune=z13")
#pragma GCC target ("tune=z10")
void fn_pragma_1_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 tu4" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_0 wd1" } } */
#pragma GCC reset_options

/**
 **
 ** Pragma and attribute
 **
 **/

#pragma GCC target ("tune=z13")
__attribute__ ((target ("tune=z13")))
void fn_pragma_1_att_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 tu7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_1 wd1" } } */
#pragma GCC reset_options

#pragma GCC target ("tune=z13")
__attribute__ ((target ("tune=z13")))
void fn_pragma_0_att_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 tu7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_1 wd1" } } */
#pragma GCC reset_options

#pragma GCC target ("tune=z13")
__attribute__ ((target ("tune=z10")))
void fn_pragma_1_att_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 tu4" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_1_att_0 wd1" } } */
#pragma GCC reset_options

#pragma GCC target ("tune=z13")
__attribute__ ((target ("tune=z10")))
void fn_pragma_0_att_0 (void) { }
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 tu4" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 sg0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 bc2" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 wf0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 hd1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 ba0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 hf1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 vx1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 ht1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 ps1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 se0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 zv1" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 mv0" } } */
/* { dg-final { scan-assembler "fn:fn_pragma_0_att_0 wd1" } } */
#pragma GCC reset_options

/**
 **
 ** End
 **
 **/

void fn_default_end (void) { }
/* { dg-final { scan-assembler "fn:fn_default_end ar7" } } */
/* { dg-final { scan-assembler "fn:fn_default_end tu4" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ss4096" } } */
/* { dg-final { scan-assembler "fn:fn_default_end sg0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end bc2" } } */
/* { dg-final { scan-assembler "fn:fn_default_end wf0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end hd1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ba0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end hf1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end vx1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ht1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end ps1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end se0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end zv1" } } */
/* { dg-final { scan-assembler "fn:fn_default_end mv0" } } */
/* { dg-final { scan-assembler "fn:fn_default_end wd1" } } */
