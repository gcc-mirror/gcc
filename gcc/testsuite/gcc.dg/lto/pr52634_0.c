/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -r -flto-partition=1to1 } } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
extern int cfliteValueCallBacks;
void baz (int *);
int main () { baz(&cfliteValueCallBacks); }
