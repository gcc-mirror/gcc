// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-g -dA" }
// { dg-final { scan-assembler "DW_TAG_template_value_param" } }
// { dg-final { scan-assembler "f.*DW_AT_name" } }
// { dg-final { scan-assembler "_Z4blehv.*DW_AT_const_value" } }

typedef void (*func_ptr)();

template <func_ptr f>
int
func()
{
    f();
    return 0;
}

void
bleh()
{
}

int c = func<bleh>();

