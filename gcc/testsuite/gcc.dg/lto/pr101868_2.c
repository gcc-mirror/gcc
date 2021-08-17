typedef unsigned long VALUE;

static void thing(void) {}
static void (*ptr)(void) = &thing;

VALUE
rb_donothing(VALUE klass)
{
        ptr();
        return 0;
}
