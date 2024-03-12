// { dg-do compile }
// { dg-options "-O2" }

int g_variant_type_info_basic_table[1];
int g_variant_type_info_check__g_boolean_var_, g_variant_type_info_get_index;
int *g_variant_type_info_get_info;
int g_assertion_message_expr();
void g_variant_type_info_check(int *info) {
  int index = info - g_variant_type_info_basic_table;
  if (index)
    g_variant_type_info_check__g_boolean_var_ = 1;
  g_assertion_message_expr();
}
void g_variant_type_info_get() {
  g_variant_type_info_get_info =
      g_variant_type_info_basic_table + g_variant_type_info_get_index;
  g_variant_type_info_check(g_variant_type_info_get_info);
}
