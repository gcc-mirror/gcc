/* Reduced from a case in haproxy's cfgparse.c where -fanalyzer
   erroneously considered the __atomic_exchange_n (&i, 1, 0) to
   affect "cookie_len" rather than "i".  */

extern int cookie_len;

void
check_config_validity ()
{
  static char i;

  if (!cookie_len)
    cookie_len = 64;

  while (1)
    {
      __atomic_exchange_n (&i, 1, 0); /* { dg-warning "infinite loop" } */
    }
}
