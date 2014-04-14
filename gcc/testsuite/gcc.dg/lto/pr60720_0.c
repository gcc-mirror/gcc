/* { dg-lto-do run } */
/* { dg-extra-ld-options { -w } } */

/* ???  lto.exp does not allow to scan for
   :1:12: warning: type of 'x' does not match original declaration
    extern int x[];
               ^
   :1:5: note: previously declared here
    int x;
        ^  */

extern int x[];
int *foo[] = { &x[0] };

int main() { return *foo[0]; }
