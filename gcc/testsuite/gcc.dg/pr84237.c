/* PR middle-end/84237 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "" } */

const char __attribute__((__section__(".bss.page_aligned.const"), __aligned__(4096))) zero_page[4096];
