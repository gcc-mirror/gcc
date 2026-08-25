#include <stdlib.h>

void test_valid_atoi(void) {
    int val = atoi("123"); /* Should be fine. */
}

void test_uninitialized_atoi(void) {
    char buf[10];
    int val = atoi(buf); /* { dg-warning "use of uninitialized value" } */
}

void test_atoi_no_lhs(void) {
    atoi("123"); /* Should be fine, shouldn't crash. */
}

void test_unterminated_atoi(void) {
    char buf[3] = {'1', '2', '3'};
    int val = atoi(buf); /* { dg-warning "over-read" } */
}

void test_valid_atol(void) {
    long val = atol("123"); /* Should be fine. */
}

#ifndef __AVR__
void test_valid_atoll(void) {
    long long val = atoll("123"); /* Should be fine. */
}
#endif
