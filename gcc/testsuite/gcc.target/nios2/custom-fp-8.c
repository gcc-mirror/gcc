/* Test whitespace skipping in target attributes.  */

/* { dg-do compile } */

#pragma GCC target ("custom-fdivs=246")
#pragma GCC target (" custom-fdivs=246")
#pragma GCC target ("custom-fdivs =246")
#pragma GCC target ("custom-fdivs= 246")
#pragma GCC target ("custom-fdivs=246 ")

#pragma GCC target ("custom-fdivs=246,custom-fabss=247")
#pragma GCC target ("custom-fdivs=246 ,custom-fabss=247")
#pragma GCC target ("custom-fdivs=246, custom-fabss=247")
#pragma GCC target ("custom-fdivs=246 , custom-fabss=247")

void foo (void) __attribute__ ((target ("custom-fcmpnes=226,custom-fcmpeqs=227")));
void foo (void) __attribute__ ((target ("custom-fcmpnes =226 ,custom-fcmpeqs=227")));
void foo (void) __attribute__ ((target ("custom-fcmpnes= 226, custom-fcmpeqs=227")));
void foo (void) __attribute__ ((target (" custom-fcmpnes=226 , custom-fcmpeqs = 227")));
void foo (void) __attribute__ ((target (" custom-fcmpnes=226 ,custom-fcmpeqs =227 ")));

#pragma GCC target ("custom-fpu-cfg=60-1")
#pragma GCC target ("custom-fpu-cfg =60-1 ")
#pragma GCC target (" custom-fpu-cfg= 60-1 ")
