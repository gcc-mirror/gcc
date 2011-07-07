/* PR middle-end/31490 */
/* { dg-do compile } */
/* { dg-require-named-sections "" } */
/* { dg-require-effective-target ptr32plus } */

int cpu (void *attr) {}
const unsigned long x __attribute__((section("foo"))) =  (unsigned long)&cpu;
const unsigned long g __attribute__((section("foo"))) = 0;
