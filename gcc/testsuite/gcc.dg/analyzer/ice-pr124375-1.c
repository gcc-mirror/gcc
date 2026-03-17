/* { dg-additional-options "-O2" } */

char __printf_buffer_spec;
int __printf_buffer_offset;
void *__printf_buffer_ptr;
void __printf_buffer() {
  int step0_jumps[] = {&&do_flag_hash - &&do_form_unknown};
do_flag_hash:
  __printf_buffer_offset
    = __printf_buffer_spec ? &&do_form_unknown - &&do_form_unknown : step0_jumps[' ']; /* { dg-warning "stack-based buffer over-read" } */
  __printf_buffer_ptr = &&do_form_unknown + __printf_buffer_offset;
  goto *__printf_buffer_ptr;
do_form_unknown:
}
