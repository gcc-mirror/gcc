/* { dg-require-effective-target vect_int } */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */

#include "tree-vect.h"

typedef unsigned char u8;
typedef unsigned int u32;

typedef struct {
    u32 pad; // important
    u8  buf[64];
} s_t;

__attribute__((noipa))
void h_bad(s_t *state, const u8 *in, u32 inlen) {
    for (u32 i = 0; i < inlen; i++) {
        state->buf[i] = in[i];
    }
}

__attribute__((noipa))
void h_good(s_t *state, const u8 *in, u32 inlen) {
    for (u32 i = 0; i < inlen; i++) {
        asm volatile("" ::: "memory"); // break vectorizer
        state->buf[i] = in[i];
    }
}

void h(s_t *state, const u8 *in, u32 inlen) {
    s_t s1 = {};
    s_t s2 = {};
    h_good(&s1, in, inlen);
    h_bad(&s2, in, inlen);
    if (__builtin_memcmp(&s1, &s2, sizeof(s1)) != 0) {
        __builtin_trap();
    }
}

int main(void) {

   check_vect ();

    s_t s = {};
    const u8 in[] =
        "0123456789" /* 0 */
        " 0123456789"
        " 0123456789"
        " 0123456789"
        " 0123456789"
    ;
    for (u32 o = 0; o < 10; o++) {
        __builtin_memset(&s, 0, sizeof(s));
        h(&s, &in[o], 33);
    }
}
