/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

#if __has_embed (__FILE__ limit (1)) != 1
#error "__has_embed failed"
#endif

int a =
#embed __FILE__ limit (1) /* { dg-warning "'#embed' is a C23 feature" } */
;
int b =
(__extension__
#embed __FILE__ limit (1)
);
