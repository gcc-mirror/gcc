/* { dg-do run } */
/* PR tree-optimization/121279 */

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
   because the bool load from the union is pulled out of the
   loop but that was conditional before and now it is not,
   so turns it into  `s_item_mem.type == 1 | bool` which is
   not valid if `s_item_mem.type == 2` . */
static bool truthy(void) __attribute__((noipa));
static bool
truthy(void)
{
    bool tt = false;
    for(int i = 0; i < 10; i++)
    {
      if (s_item_mem.type == 0)
        tt = tt | 0;
      else if (s_item_mem.type == 1)
        tt = tt | s_item_mem.boolean;
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
