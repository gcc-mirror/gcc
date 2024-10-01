// { dg-do run }
/* PR tree-optimization/116098 */


static bool truthy(int type, unsigned char data) __attribute__((noipa));
/* truthy was being miscompiled for the type==2 case,
   because we would have a VCE from unsigned char to bool
   that went from being conditional in the type==1 case
   to unconditional when `type!=0`.
   The move of the VCE from conditional to unconditional,
   needs to changed into a convert (NOP_EXPR). */

static bool truthy(void) __attribute__((noipa));
static bool
truthy(int type, unsigned char data)
{
    if (type == 0)
      return 0;
    if (type == 1)
      /* Emulate what SRA does, so this can be
	 tested without depending on SRA. */
      return __builtin_bit_cast (bool, data);
    return 1;
}

int
main(void)
{
    bool b1 = !truthy(2, -1);
    bool b = truthy(1, b1);
    if (b1 != b)  __builtin_abort();
    if (b) __builtin_abort();
}
