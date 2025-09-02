/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

#include <stddef.h>
#include <stdlib.h>
typedef struct {
    int data[5];
    nullptr_t np;
} container_t;
void process_array(int *arr, size_t len, nullptr_t nullp) {
    for (size_t i = 0; i < len; ++i) {
        switch (arr[i] % 4) {
            case 1:
                if (nullp == nullptr) {
                    arr[i] *= -1;
                    [[fallthrough]];
                }
            case 2:
                arr[i] = abs(arr[i]);
                break;
            default:
                arr[i] = 0;
        }
    }
}
int main(void) {
    container_t c = {
        .data = { -3, 1, 4, 2, 7 },
        .np = nullptr
    };
    process_array(c.data, 5, c.np);
}
