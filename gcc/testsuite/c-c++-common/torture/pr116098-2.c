/* { dg-do run } */
/* PR tree-optimization/116098 */


#include <stdbool.h>

struct Value {
    int type;
    union {
        bool boolean;
        long long t;
    };
};

static struct Value s_item_mem;

/* truthy was being miscompiled for the value.type==2 case,
   because we would have a VCE from unsigned char to bool
   that went from being conditional in the value.type==1 case
   to unconditional when `value.type!=0`.
   The move of the VCE from conditional to unconditional,
   needs to changed into a convert (NOP_EXPR). */
static bool truthy(void) __attribute__((noipa));
static bool
truthy(void)
{
    struct Value value = s_item_mem;
    if (value.type == 0)
      return 0;
    if (value.type == 1)
      return value.boolean;
    return 1;
}

int
main(void)
{
    s_item_mem.type = 2;
    s_item_mem.t = -1;
    bool b1 = !truthy();
    s_item_mem.type = 1;
    s_item_mem.boolean = b1;
    bool b = truthy();
    if (b1 != b)  __builtin_abort();
    if (b) __builtin_abort();
}
