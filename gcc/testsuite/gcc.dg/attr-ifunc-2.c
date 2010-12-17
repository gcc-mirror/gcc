/* { dg-require-ifunc "" } */

static void *resolver ()
{
  return 0;
}

extern int magic (void)  /* { dg-message "previous definition" } */
     __attribute__ ((ifunc ("resolver")));
extern int magic (void)  /* { dg-error "redefinition" "" } */
     __attribute__ ((alias ("resolver")));

extern int spell (void)  /* { dg-message "previous definition" } */
{
  return 0;
}
extern int spell (void)  /* { dg-error "redefinition" "" } */
     __attribute__ ((ifunc ("resolver")));

extern int mantra (void)  /* { dg-message "previous definition" } */
     __attribute__ ((alias ("resolver")));
extern int mantra (void)  /* { dg-error "redefinition" "" } */
     __attribute__ ((ifunc ("resolver")));

extern int saying (void)  /* { dg-error "weak .* cannot be defined" "" } */
     __attribute__ ((weak,ifunc ("resolver")));
extern int maxim (void) /* { dg-error "indirect function .* cannot be declared weak" "" } */
     __attribute__ ((ifunc ("resolver"),weak));
