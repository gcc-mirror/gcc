/* { dg-do compile { target pie_enabled } } */
/* { dg-options "" } */

#ifndef __PIC__
# error __PIC__ is not defined!
#endif

#ifndef __PIE__
# error __PIE__ is not defined!
#endif
