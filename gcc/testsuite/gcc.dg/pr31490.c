/* PR middle-end/31490 */
/* { dg-do compile } */
/* { dg-require-named-sections "" } */
/* { dg-require-effective-target ptr32plus } */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

int cpu (void *attr) {}
const uintptr_t x __attribute__((section("foo"))) =  (uintptr_t)&cpu;
const uintptr g __attribute__((section("foo"))) = 0;
