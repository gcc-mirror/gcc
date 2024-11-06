/* { dg-do compile } */
/* { dg-options "-std=c17 -pedantic-errors -Wall -W" } */

#if __has_embed (__FILE__ limit (1)) != 1
#error "__has_embed failed"
#endif

int a =
#embed __FILE__ limit (1) /* { dg-error "'#embed' before C23 is a GCC extension" } */
;
int b =
(__extension__
#embed __FILE__ limit (1)
);
