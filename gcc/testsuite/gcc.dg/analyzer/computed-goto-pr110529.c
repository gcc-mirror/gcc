/* C only: reuse of same array for int and label pointers.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

void foo(int pc) {
    int *arr[2] = {&&x, &&y};
    int var = 0;
    __analyzer_dump_path (); /* { dg-message "path" } */
    
    goto *arr[pc];

x:
    __analyzer_dump_path (); /* { dg-message "path" } */
    __analyzer_eval (pc == 0); /* { dg-warning "TRUE" } */
    /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
    arr[0] = (void *)0;
    *arr[0] = 10086; /* { dg-warning "dereference of NULL" } */
    return;
y:
    __analyzer_dump_path (); /* { dg-message "path" } */
    __analyzer_eval (pc == 1); /* { dg-warning "TRUE" "" { xfail *-*-* } } */
    /* { dg-bogus "FALSE" "" { target *-*-* } .-1 } */
    /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-2 } */
    return;
}

int main() { foo(0); }
