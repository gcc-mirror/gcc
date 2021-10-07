/* { dg-do compile } */
/* { dg-options "-O2" } */

_Bool _bfd_elf_merge_symbol_h, _bfd_elf_merge_symbol_h_1;
_Bool _bfd_elf_merge_symbol_olddef;
_Bool bfd_is_com_section();

void
_bfd_elf_merge_symbol() {
  _Bool newdef = bfd_is_com_section(), ntdef, tdef;
  _bfd_elf_merge_symbol_olddef = _bfd_elf_merge_symbol_h;
  if (_bfd_elf_merge_symbol_h_1) {
    ntdef = newdef;
    tdef = _bfd_elf_merge_symbol_h;
  } else {
    ntdef = _bfd_elf_merge_symbol_h;
    tdef = newdef;
  }
  if (tdef && ntdef)
    ;
}
