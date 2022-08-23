/* { dg-lto-do run } */
/* { dg-lto-options { "-O2 -fno-strict-aliasing -flto" } } */

typedef unsigned long VALUE;

__attribute__ ((cold))
void rb_check_type(VALUE, int);

static VALUE
repro(VALUE dummy, VALUE hash)
{
    if (hash == 0) {
        rb_check_type(hash, 1);
    }
    else if (*(long *)hash) {
        rb_check_type(hash, 1);
    }


    return *(long *)hash;
}

static VALUE (*that)(VALUE dummy, VALUE hash) = repro;

volatile int zero = 0;

int
main(int argc, char **argv)
{
        that(0, zero);

        rb_check_type(zero, zero);

}
