typedef unsigned long VALUE;


__attribute__ ((noreturn)) void rexc_raise(VALUE mesg);

VALUE rb_donothing(VALUE klass);

static void
funexpected_type(VALUE x, int xt, int t)
{
    rexc_raise(rb_donothing(0));
}

__attribute__ ((cold))
void
rb_check_type(VALUE x, int t)
{
    int xt;

    if (x == 0) {
        funexpected_type(x, xt, t);
    }
}
