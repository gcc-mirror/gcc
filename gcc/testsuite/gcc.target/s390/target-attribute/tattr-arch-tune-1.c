/* Functional tests for the "target" attribute and pragma.  */

/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mdebug -march=z13" } */

/**
 ** no pragma
 **/

void fn_p0_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_p0_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_p0_1 tu7" } } */

__attribute__ ((target("arch=zEC12")))
void fn_p0_2 (void) { }
/* { dg-final { scan-assembler "fn:fn_p0_2 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_p0_2 tu6" } } */

__attribute__ ((target("tune=z196")))
void fn_p0_3 (void) { }
/* { dg-final { scan-assembler "fn:fn_p0_3 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_p0_3 tu5" } } */

__attribute__ ((target("arch=zEC12,tune=z196")))
void fn_p0_4 (void) { }
/* { dg-final { scan-assembler "fn:fn_p0_4 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_p0_4 tu5" } } */

__attribute__ ((target("tune=z196,arch=zEC12")))
void fn_p0_5 (void) { }
/* { dg-final { scan-assembler "fn:fn_p0_5 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_p0_5 tu5" } } */

/**
 ** arch pragma
 **/

#pragma GCC target ("arch=z9-ec")

void fn_pa_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pa_1 ar3" } } */
/* { dg-final { scan-assembler "fn:fn_pa_1 tu3" } } */

__attribute__ ((target("arch=zEC12")))
void fn_pa_2 (void) { }
/* { dg-final { scan-assembler "fn:fn_pa_2 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pa_2 tu6" } } */

__attribute__ ((target("tune=z196")))
void fn_pa_3 (void) { }
/* { dg-final { scan-assembler "fn:fn_pa_3 ar3" } } */
/* { dg-final { scan-assembler "fn:fn_pa_3 tu5" } } */

__attribute__ ((target("arch=zEC12,tune=z196")))
void fn_pa_4 (void) { }
/* { dg-final { scan-assembler "fn:fn_pa_4 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pa_4 tu5" } } */

__attribute__ ((target("tune=z196,arch=zEC12")))
void fn_pa_5 (void) { }
/* { dg-final { scan-assembler "fn:fn_pa_5 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pa_5 tu5" } } */

#pragma GCC reset_options

/**
 ** tune pragma
 **/

#pragma GCC target ("tune=z9-109")

void fn_pt_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pt_1 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pt_1 tu2" } } */

__attribute__ ((target("arch=zEC12")))
void fn_pt_2 (void) { }
/* { dg-final { scan-assembler "fn:fn_pt_2 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pt_2 tu2" } } */

__attribute__ ((target("tune=z196")))
void fn_pt_3 (void) { }
/* { dg-final { scan-assembler "fn:fn_pt_3 ar7" } } */
/* { dg-final { scan-assembler "fn:fn_pt_3 tu5" } } */

__attribute__ ((target("arch=zEC12,tune=z196")))
void fn_pt_4 (void) { }
/* { dg-final { scan-assembler "fn:fn_pt_4 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pt_4 tu5" } } */

__attribute__ ((target("tune=z196,arch=zEC12")))
void fn_pt_5 (void) { }
/* { dg-final { scan-assembler "fn:fn_pt_5 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pt_5 tu5" } } */

#pragma GCC reset_options

/**
 ** arch and tune pragmas
 **/

#pragma GCC target ("arch=z9-ec,tune=z9-109")

void fn_pat_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pat_1 ar3" } } */
/* { dg-final { scan-assembler "fn:fn_pat_1 tu2" } } */

__attribute__ ((target("arch=zEC12")))
void fn_pat_2 (void) { }
/* { dg-final { scan-assembler "fn:fn_pat_2 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pat_2 tu2" } } */

__attribute__ ((target("tune=z196")))
void fn_pat_3 (void) { }
/* { dg-final { scan-assembler "fn:fn_pat_3 ar3" } } */
/* { dg-final { scan-assembler "fn:fn_pat_3 tu5" } } */

__attribute__ ((target("arch=zEC12,tune=z196")))
void fn_pat_4 (void) { }
/* { dg-final { scan-assembler "fn:fn_pat_4 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pat_4 tu5" } } */

__attribute__ ((target("tune=z196,arch=zEC12")))
void fn_pat_5 (void) { }
/* { dg-final { scan-assembler "fn:fn_pat_5 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pat_5 tu5" } } */

#pragma GCC reset_options

/**
 ** tune and arch pragmas
 **/

#pragma GCC target ("tune=z9-109,arch=z9-ec")

void fn_pta_1 (void) { }
/* { dg-final { scan-assembler "fn:fn_pta_1 ar3" } } */
/* { dg-final { scan-assembler "fn:fn_pta_1 tu2" } } */

__attribute__ ((target("arch=zEC12")))
void fn_pta_2 (void) { }
/* { dg-final { scan-assembler "fn:fn_pta_2 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pta_2 tu2" } } */

__attribute__ ((target("tune=z196")))
void fn_pta_3 (void) { }
/* { dg-final { scan-assembler "fn:fn_pta_3 ar3" } } */
/* { dg-final { scan-assembler "fn:fn_pta_3 tu5" } } */

__attribute__ ((target("arch=zEC12,tune=z196")))
void fn_pta_4 (void) { }
/* { dg-final { scan-assembler "fn:fn_pta_4 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pta_4 tu5" } } */

__attribute__ ((target("tune=z196,arch=zEC12")))
void fn_pta_5 (void) { }
/* { dg-final { scan-assembler "fn:fn_pta_5 ar6" } } */
/* { dg-final { scan-assembler "fn:fn_pta_5 tu5" } } */

#pragma GCC reset_options
