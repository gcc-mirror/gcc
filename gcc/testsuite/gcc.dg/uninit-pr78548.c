/* { dg-do compile } */
/* { dg-options "-Wall -w -O2" } */

char a;
int b;
unsigned c, d;
short e;
int main_f;
int main (  ) {
L0:
    if ( e )     goto L1;
    b = c & d || a;
    if ( !c )     __builtin_printf ( "", ( long long ) main_f );
    if ( d || !c )     {
        __builtin_printf ( "%llu\n", ( long long ) main );
        goto L2;
    }
    unsigned g = b;
L1:
    b = g;
L2:
    if ( b )     goto L0;
  return 0;
}
