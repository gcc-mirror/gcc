// { dg-do assemble  }
// GROUPS passed array-bindings

extern "C" int printf (const char *, ...);
char array[~(~((__SIZE_TYPE__)0ul)>>1)|~(((__SIZE_TYPE__)0ul)>>3)];  // { dg-error "39:exceeds maximum object size" } overflow in array dimension.*
int main () { printf ("PASS\n"); return 0; }
