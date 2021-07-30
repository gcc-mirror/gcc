/* { dg-do compile } */
/* { dg-options "-O2 -march=amdfam10 -mno-lzcnt -mno-popcnt" } */

#ifdef __LZCNT__
# error LZCNT should be disabled
#endif

#ifdef __POPCNT__
# error POPCNT should be disabled
#endif
