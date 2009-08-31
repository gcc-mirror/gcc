// Contributed by Dodji Seketeli <dodji@redhat.com>
// origin PR debug/30161
// { dg-options "-g -dA" }
// { dg-do compile }
// { dg-final { scan-assembler "DW_TAG_template_value_param" } }
// { dg-final { scan-assembler "i.*DW_AT_name" } }
// { dg-final { scan-assembler "3.*DW_AT_const_value" } }


template <int i>
int
func()
{
    int j = i;
    return j;
}

const int foo = 1;
const int bar = 2;

int h = func<foo+bar>();

