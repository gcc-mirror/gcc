typedef unsigned long VALUE;

__attribute__((noreturn))
void
rexc_raise(VALUE mesg)
{
        __builtin_exit(0);
}
