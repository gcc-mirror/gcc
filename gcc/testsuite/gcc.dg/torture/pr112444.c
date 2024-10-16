/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17 -ftrivial-auto-var-init=zero" } */

int qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_offset,
    qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_init_offset,
    qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_error,
    qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_phonebooks_i;

int g_string_new(), g_string_append_len(), qmi_message_tlv_read_guint8();

void qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message() {
  int printable = g_string_new();
  for (;;) {
    {
      if (__builtin_expect(({
                             int _g_boolean_var_4;
                             if (printable)
                               _g_boolean_var_4 = 1;
                             else
                               _g_boolean_var_4 = 0;
                             _g_boolean_var_4;
                           }),
                           0))
        g_string_append_len();
    }
    unsigned char tmp;
    qmi_message_tlv_read_guint8(
        qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_init_offset,
        qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_offset,
        tmp,
        qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_error);
    {
      {
        if (__builtin_expect(({
                               int _g_boolean_var_4;
                               if (printable)
                                 _g_boolean_var_4 = 1;
                               else
                                 _g_boolean_var_4 = 0;
                               _g_boolean_var_4;
                             }),
                             0))
          g_string_append_len();
      }
    }
    for (
        ;
        qmi_message_pbm_get_all_capabilities_output_capability_basic_information_get_printable_message_phonebooks_i;) {
      unsigned char tmp;
      qmi_message_tlv_read_guint8(&tmp);
      {
        if (__builtin_expect(({
                               int _g_boolean_var_4;
                               if (printable)
                                 _g_boolean_var_4 = 1;
                               else
                                 _g_boolean_var_4 = 0;
                               _g_boolean_var_4;
                             }),
                             0))
          g_string_append_len();
      }
    }
  }
}
