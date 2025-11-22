/* { dg-do compile } */

char _strtoimax_r_c;
void _strtoimax_r() {
  for (;; _strtoimax_r_c++) {
    if (_strtoimax_r_c <= '9')
      _strtoimax_r_c -= '0';
    if (_strtoimax_r_c >= 'A')
      break;
  }
}
