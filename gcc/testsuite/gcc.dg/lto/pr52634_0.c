/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -r -nostdlib -flto-partition=1to1}} */
extern int cfliteValueCallBacks;
void baz (int *);
int main () { baz(&cfliteValueCallBacks); }
