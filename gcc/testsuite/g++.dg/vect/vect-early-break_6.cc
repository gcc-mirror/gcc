// { dg-do compile }
// ICE in verify_dominators, reduced from charset.cc (libstdc++).

void convert_escape(int *);
int cpp_interpret_string_1_to, cpp_interpret_string_1_tbuf;
char *cpp_interpret_string_1_base;
char cpp_interpret_string_1_limit;
void cpp_interpret_string_1() {
  char *p;
  for (;;) {
    cpp_interpret_string_1_base = p;
    while (p < &cpp_interpret_string_1_limit && *p)
      p++;
    if (p > cpp_interpret_string_1_base)
      if (cpp_interpret_string_1_to)
        goto fail;
    if (p >= &cpp_interpret_string_1_limit)
      break;
    int *tbuf_ptr =
        cpp_interpret_string_1_to ? &cpp_interpret_string_1_tbuf : __null;
    convert_escape(tbuf_ptr);
  }
fail:
  ;
}
