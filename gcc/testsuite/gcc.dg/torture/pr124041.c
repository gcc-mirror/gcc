/* { dg-compile } */
/* { dg-additional-options "-O2  -fno-strict-aliasing -fstack-protector -ftrivial-auto-var-init=zero -fsanitize=thread -Wall" } */

char *trick_compiler(void *);
int do_usercopy_stack_callee_i;
char *do_usercopy_stack_callee(long value)
{
    char buf[28];
    for (; do_usercopy_stack_callee_i < sizeof(buf); do_usercopy_stack_callee_i++)
        buf[do_usercopy_stack_callee_i] = value;
    return trick_compiler(buf);
}
void do_usercopy_stack(void)
{
    char *bad_stack;
    bad_stack = do_usercopy_stack_callee((long)&bad_stack);
}
