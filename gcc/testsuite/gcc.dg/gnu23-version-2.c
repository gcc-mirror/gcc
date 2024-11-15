/* Test __STDC_VERSION__ for C23 with GNU extensions.  Test that -std=gnu23 is
   the default (replace this test when updating the default to a later version
   than gnu23).  */
/* { dg-do compile } */
/* { dg-options "" } */

#if __STDC_VERSION__ == 202311L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
