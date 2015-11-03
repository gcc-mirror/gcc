int pid_count = 0;

unsigned int getopt (int, const char**, const char*);
unsigned long long atoll (const char*);
int fork (void);
void kill (int, int);

int
main (int argc, const char *argv[])
{
  unsigned int c;
  unsigned long long maxbytes = 0;
  extern const char *optarg;
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
