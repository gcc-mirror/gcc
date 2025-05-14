/* { dg-do run } */
/* PR tree-optimization/120122 */

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
    bool tt = false;
    for(int i = 0; i < 10; i++)
    {
      struct Value value = s_item_mem;
      if (value.type == 0)
        tt = tt | 0;
      else if (value.type == 1)
        tt = tt |  value.boolean;
      else
        tt = tt |  1;
    }
    return tt;
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
