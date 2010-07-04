/* { dg-do compile } */
/* { dg-options "-O2 -m4" } */

int pid_count = 0;
main (int argc, char *argv[])
{
  unsigned int c;
  unsigned long long maxbytes = 0;
  extern char *optarg;
  int i;
  int pid_cntr;
  int pid;
  int pid_list[1000];
  while ((c = getopt (argc, argv, "c:b:p:wvh")) != (-1))
    {
      switch ((char) c)
	{
	case 'b':
	  maxbytes = atoll (optarg);
	}
    }
  pid = fork ();
  while ((pid != 0) && (maxbytes > 1024 * 1024 * 1024))
    {
      maxbytes = maxbytes - (1024 * 1024 * 1024);
      pid = fork ();
      if (pid != 0)
	pid_cntr++;
      pid_list[i] = pid;
    }
  while ((pid_count < pid_cntr))
    {
    }
  kill (pid_list[i], 9);
}

