#if defined(AVR) /* flag_delete_null_pointer_checks = 0  */
int sc = (&sc >= 0);
#else
int sc = (&sc > 0);
#endif

