/* { dg-do run } */

#include <assert.h>

#define CONTAINER_KIND union

typedef CONTAINER_KIND container { int value; } container;

void move(container* end, container* start) {
    container* p;
    for (p = end; p > start; p--) {
	(p)->value = (p-1)->value;
    }
}

#define N 100

int main(int argc, char* argv[]) {
    container vals[N];
    int i;
    for (i=0; i<N; i++) {
        vals[i].value = argc + i;
    }
    move(&vals[N-1], &vals[0]);
    assert(vals[0].value == argc + 0);
    for (i=1; i<N; i++) {
        assert(vals[i].value == argc + i - 1);
    }
    return 0;
}
