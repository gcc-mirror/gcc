/* { dg-do run } */
/* { dg-options "-fbuilding-libgcc" } */

static unsigned char dwarf_reg_size_table[__LIBGCC_DWARF_FRAME_REGISTERS__+1];

int
main (void)
{
  __builtin_init_dwarf_reg_size_table (dwarf_reg_size_table);
  /* X0-X31 and SP.  */
  for (int i = 0; i < 32; ++i)
    if (dwarf_reg_size_table[i] != 8)
      __builtin_abort ();
  /* Q0-Q31/Z0-Z31, of which only the low 64 bits of register 8-15
     are saved.  */
  for (int i = 64; i < 96; ++i)
    if (dwarf_reg_size_table[i] != (i >= 72 && i < 80 ? 8 : 0))
      __builtin_abort ();
  /* P0-P15, which are never saved.  */
  for (int i = 48; i < 63; ++i)
    if (dwarf_reg_size_table[i] != 0)
      __builtin_abort ();
  /* VG */
  if (dwarf_reg_size_table[46] != 8)
    __builtin_abort ();
  return 0;
}
