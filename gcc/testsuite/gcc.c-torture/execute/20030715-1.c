/* PR optimization/11320 */
/* Origin: Andreas Schwab <schwab@suse.de> */

/* Verify that the scheduler correctly computes the dependencies
   in the presence of conditional instructions.  */

int strcmp (const char *, const char *);
int ap_standalone;

const char *ap_check_cmd_context (void *a, int b)
{
  return 0;
}

const char *server_type (void *a, void *b, char *arg)
{
  const char *err = ap_check_cmd_context (a, 0x01|0x02|0x04|0x08|0x10);
  if (err)
    return err;

  if (!strcmp (arg, "inetd"))
    ap_standalone = 0;
  else if (!strcmp (arg, "standalone"))
      ap_standalone = 1;
  else
    return "ServerType must be either 'inetd' or 'standalone'";

  return 0;
}

int main ()
{
  server_type (0, 0, "standalone");
  return 0;
}
