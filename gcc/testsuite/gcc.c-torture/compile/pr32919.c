void _IO_vfprintf_internal ( char *f )
{
  static const void *const step0_jumps[] = { &&do_form_unknown, &&do_flag_plus, &&do_form_float };
  const void * ptr = step0_jumps[0];
  do {
    char spec;
    spec = (*++f);
    goto *ptr;
do_flag_plus:
     read_int (&f);
do_number:
    _itoa_word (spec);
do_form_float:
    if (ptr != ((void *)0))
    {
      spec = 'x';
      goto do_number;
    }
    if (spec != 'S')
      __strnlen ();
    return;
    do_form_unknown:;
  }
  while (*f != '\0');
}

