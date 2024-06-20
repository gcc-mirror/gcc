#include <stdio.h>

__attribute__((target_clones("default", "sse4.2", "avx2")))
void bar() {
    printf("Function bar\n");
}

int main() {
    bar();
    return 0;
}
