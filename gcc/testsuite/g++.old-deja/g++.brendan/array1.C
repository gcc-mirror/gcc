// Build don't link: 
// Special g++ Options: -fconserve-space -fcommon
// GROUPS passed array-bindings
// excess errors test - XFAIL sparc64-*-* 
extern "C" void printf (char *, ...);
char array[(unsigned) 0x90000000];// ERROR -  overflow in array dimension.* , XFAIL sparc64-*-* alpha*-*-*
int main () { printf ("PASS\n"); return 0; }
