// Build don't link: 
// Special g++ Options: -fconserve-space -fcommon
// GROUPS passed array-bindings

extern "C" int printf (const char *, ...);
char array[~(~0ul>>1)|~(0ul>>3)];  // ERROR - overflow in array dimension.*
int main () { printf ("PASS\n"); return 0; }
