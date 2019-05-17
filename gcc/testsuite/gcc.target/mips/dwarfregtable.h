
typedef unsigned Unwind_Word __attribute__((__mode__(__unwind_word__)));

#define DWARF_FRAME_REGISTERS 188

static unsigned char ref_dwarf_reg_size_table[DWARF_FRAME_REGISTERS + 1] =
  {
    [0 ... 66] =  sizeof (Unwind_Word),
    [80 ... 181] = sizeof (Unwind_Word)
  };

static unsigned char dwarf_reg_size_table[DWARF_FRAME_REGISTERS + 1] = {};

int
main (void)
{
  __builtin_init_dwarf_reg_size_table (dwarf_reg_size_table);
  if (__builtin_memcmp (ref_dwarf_reg_size_table,
                       dwarf_reg_size_table, DWARF_FRAME_REGISTERS + 1) != 0)
    __builtin_abort ();
  return 0;
}
