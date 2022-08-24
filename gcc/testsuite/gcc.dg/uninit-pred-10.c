/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

enum demangle_component_type {
  DEMANGLE_COMPONENT_NAME,
  DEMANGLE_COMPONENT_REFERENCE
};
struct demangle_component {
  enum demangle_component_type type;
} d_print_comp_inner_mod_inner, *d_print_comp_inner_dc;
struct d_print_mod {
  struct d_print_mod *next;
};
struct d_print_info {
  int templates;
  struct d_print_mod *modifiers;
};
void d_print_comp_inner(struct d_print_info *dpi)
{
  int saved_templates, need_template_restore = 0;
  switch (d_print_comp_inner_dc->type) {
  case DEMANGLE_COMPONENT_NAME:
    goto modifier;
  case DEMANGLE_COMPONENT_REFERENCE:
    saved_templates = dpi->templates;
    need_template_restore = 1;
  modifier:
    struct d_print_mod dpm;
    dpm.next = dpi->modifiers;
    d_print_comp_inner_mod_inner = *d_print_comp_inner_dc;
    d_print_comp_inner(dpi);
    dpi->modifiers = dpm.next;
    if (need_template_restore)
      dpi->templates = saved_templates; /* { dg-bogus "uninitialized" } */
  }
}
