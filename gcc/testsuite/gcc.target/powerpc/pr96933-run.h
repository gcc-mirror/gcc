/* Test function for pr96933-{3,4}.c run result verification.  */

int
main ()
{
  unsigned char uc[16];
  signed char sc[16];

  for (int i = 0; i < 16; i++)
    {
      uc[i] = (unsigned char) (i * 2 + 1);
      sc[i] = (signed char) ((i % 2 == 0) ? (i + 1) : -i);
    }

  vector unsigned char ucv
    = test_uchar (uc[0], uc[1], uc[2], uc[3], uc[4], uc[5], uc[6], uc[7], uc[8],
		  uc[9], uc[10], uc[11], uc[12], uc[13], uc[14], uc[15]);
  vector signed char scv
    = test_schar (sc[0], sc[1], sc[2], sc[3], sc[4], sc[5], sc[6], sc[7], sc[8],
		  sc[9], sc[10], sc[11], sc[12], sc[13], sc[14], sc[15]);

  for (int i = 0; i < 16; i++)
    {
      unsigned char uexp = (unsigned char) (i * 2 + 1);
      signed char sexp = (signed char) ((i % 2 == 0) ? (i + 1) : -i);
      if (ucv[i] != uexp)
	abort ();
      if (scv[i] != sexp)
	abort ();
    }

  unsigned short us[8];
  signed short ss[8];
  for (int i = 0; i < 8; i++)
    {
      us[i] = (unsigned short) (i * 2 + 1);
      ss[i] = (signed short) ((i % 2 == 0) ? (i + 1) : -i);
    }

  vector unsigned short usv
    = test_ushort (us[0], us[1], us[2], us[3], us[4], us[5], us[6], us[7]);
  vector signed short ssv
    = test_sshort (ss[0], ss[1], ss[2], ss[3], ss[4], ss[5], ss[6], ss[7]);

  for (int i = 0; i < 8; i++)
    {
      unsigned short uexp = (unsigned short) (i * 2 + 1);
      signed short sexp = (signed short) ((i % 2 == 0) ? (i + 1) : -i);
      if (usv[i] != uexp)
	abort ();
      if (ssv[i] != sexp)
	abort ();
    }

  return 0;
}
