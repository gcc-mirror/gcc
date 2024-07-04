#include "analyzer-decls.h"

int main(void) {
    char buf[] = "0";
    int *ptr = (int *)(__builtin_strlen(buf) - 1);
    __analyzer_eval((__builtin_strlen(buf)) == 1); /* { dg-warning "TRUE" } */
    *ptr = 10086; /* { dg-warning "dereference of NULL 'ptr'" } */
}
