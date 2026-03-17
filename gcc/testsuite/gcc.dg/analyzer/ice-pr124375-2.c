int __wcslen_spec, __wcslen_offset;
void *__wcslen_ptr;
long __wcslen() {
  &&do_form_binary;
  __wcslen_offset =
      __wcslen_spec ? &&do_form_unknown - &&do_form_unknown : L' ';
  __wcslen_ptr = &&do_form_unknown + __wcslen_offset;
  goto *__wcslen_ptr;
do_form_binary:
do_form_unknown:
} /* { dg-warning "return-value" } */
