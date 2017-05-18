/* { dg-do compile } */

unsigned long ok = 0UL;
#pragma GCC diagnostic push
#pragma GCC diagnostic warning "-Wtraditional"
unsigned long bad = 1UL; /* { dg-warning "suffix" } */
/* Note the extra space before the pragma on this next line: */
 #pragma GCC diagnostic pop
unsigned long ok_again = 2UL; /* { dg-bogus "suffix" } */
