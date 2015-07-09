// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-gdwarf-2 -dA -gno-strict-dwarf -fno-merge-debug-strings" }
// { dg-final { scan-assembler "DW_TAG_template_value_param" } }
// { dg-final { scan-assembler "f.*DW_AT_name" } }
// { dg-final { scan-assembler "DW_AT_location\[^\\r\\n\]*\[\\r\\n\]*\[^\\r\\n\]*DW_OP_addr\[^\\r\\n\]*\[\\r\\n\]*\[^\\r\\n\]*_Z4blehv\[^\\r\\n\]*\[\\r\\n\]*\[^\\r\\n\]*DW_OP_stack_value" } } */

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

