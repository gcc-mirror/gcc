/* { dg-do compile } */
/* { dg-options "-g -O2 -fcompare-debug" } */

char *camel_message_info_class_intern_init_part;
void g_once_init_enter();
void camel_message_info_class_intern_init() {
  int ii;
  char *label;
  for (; camel_message_info_class_intern_init_part[ii]; ii++)
    if (camel_message_info_class_intern_init_part) {
      if (label && *label)
        g_once_init_enter();
      label = &camel_message_info_class_intern_init_part[ii + 1];
      camel_message_info_class_intern_init_part[ii] = ' ';
    }
  if (label)
    g_once_init_enter();
}
