/* Test for -Wtraditional warnings for stringification of macro args.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 9/8/2000.  */
/* { dg-do preprocess } */
/* { dg-options "-Wtraditional -fno-show-column" } */

#define foo1(h) sdf "h3" fds "h" /* { dg-warning "macro argument \"h\" would be stringified" "traditional stringification" } */
#define foo2(h2) sdf "h2" fds "h3" /* { dg-warning "macro argument \"h2\" would be stringified" "traditional stringification" } */
#define foo3(h3) sdf "h2" fds "h3" /* { dg-warning "macro argument \"h3\" would be stringified" "traditional stringification" } */
#define foo4(h) sdf 'h3' fds 'h' /* { dg-warning "macro argument \"h\" would be stringified" "traditional stringification" } */
#define foo5(h2) sdf 'h2' fds 'h3' /* { dg-warning "macro argument \"h2\" would be stringified" "traditional stringification" } */
#define foo6(h3) sdf 'h2' fds 'h3' /* { dg-warning "macro argument \"h3\" would be stringified" "traditional stringification" } */
#define foo7(AA, hello, world, EEE) sdf "A B hello C,world,DhelloE F" fds EEE /* { dg-warning "macro argument \"hello\" would be stringified" "traditional stringification" } */

/* Catch the second warning from the above line.  */
/* { dg-warning "macro argument \"world\" would be stringified" "traditional stringification second warning" { target *-*-* } 13 } */

# 19 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

#define bar1(h) sdf "h3" fds "h"
#define bar2(h2) sdf "h2" fds "h3"
#define bar3(h3) sdf "h2" fds "h3"
#define bar4(h) sdf 'h3' fds 'h'
#define bar5(h2) sdf 'h2' fds 'h3'
#define bar6(h3) sdf 'h2' fds 'h3'
#define bar7(AA, hello, world, EEE) sdf "A B hello C,world,DhelloE F" fds EEE
