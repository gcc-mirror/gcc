/* { dg-do run } */
/* PR tree-optimization/116098 */
/* truthy was being miscompiled where the VCE was not being pulled out
   of the if statement by factor_out_conditional_operation before the rest of
   phiopt would happen which assumed VCE would be correct. */
/* The unused label was causing truthy to have different code generation than truthy_1. */


#ifndef __cplusplus
#define bool _Bool
#endif

enum ValueType {
        VALUE_BOOLEAN,
        VALUE_NUM,
};

struct Value {
    enum ValueType type;
    union {
        bool boolean;
        int num;
    };
};

static struct Value s_value;
static bool s_b;


bool truthy_1(void) __attribute__((noinline));
bool
truthy_1(void)
{
    struct Value value = s_value;
    if (s_b) s_b = 0;
    enum ValueType t = value.type;
    if (t != VALUE_BOOLEAN)
      return 1;
  return value.boolean;
}
bool truthy(void) __attribute__((noinline));
bool
truthy(void)
{
    struct Value value = s_value;
    if (s_b) s_b = 0;
    enum ValueType t = value.type;
    if (t != VALUE_BOOLEAN)
      return 1;
  /* This unused label should not cause any difference in code generation. */
a: __attribute__((unused));
  return value.boolean;
}

int
main(void)
{
    s_b = 0;
    s_value = (struct Value) {
        .type = VALUE_NUM,
        .num = 2,
    };
    s_value = (struct Value) {
        .type = VALUE_BOOLEAN,
        .boolean = !truthy_1(),
    };
    bool b = truthy_1();
    if (b)
      __builtin_abort();

    s_b = 0;
    s_value = (struct Value) {
        .type = VALUE_NUM,
        .num = 2,
    };
    s_value = (struct Value) {
        .type = VALUE_BOOLEAN,
        .boolean = !truthy(),
    };
    b = truthy();
    if (b)
      __builtin_abort();
}

