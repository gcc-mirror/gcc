/* { dg-lto-do link } */
/* { dg-lto-options { { -fPIC -flto -flto-partition=1to1 } } } */
/* { dg-extra-ld-options { -shared } } */

static void *master;
void *foo () { return master; }
