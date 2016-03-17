/* { dg-do compile { target { ! pie_enabled } } } */
/* { dg-options "" } */

#ifdef __PIE__
# error __PIE__ is defined!
#endif
