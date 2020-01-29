/* { dg-do-compile } */
/* { dg-options "-O2 -Warray-bounds" } */

#include <string.h>
#include <assert.h>
#include <stdio.h>

static inline __attribute__((__artificial__)) void *a(char *c, const char *d, long n)
{
    return __builtin___memcpy_chk(c, d, n, __builtin_object_size(c, 0));
}
typedef struct {
    char *data;
    int len;
} sb_t;
const char __sb_slop[1];
static void inline set0(sb_t *c)
{
    if (c->data != __sb_slop)
        c->data[0] = 0;
    else
        assert (c->data[0] == 0);
}
char buf[5];
sb_t l = {
    .data = buf,
    .len = 0
};
void o()
{
    char *data = "abcd";
    sb_t h = l;
    set0(&h);
    a(h.data, data, strlen(data));
    printf("%s\n", h.data);
    printf("%d\n", h.data == __sb_slop);
    printf("%d\n", h.data == buf);
    set0(&h);
}
int main(void) {
    o();
    return 0;
}
