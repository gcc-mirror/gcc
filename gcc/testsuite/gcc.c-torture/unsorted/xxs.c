void *malloc ();
struct timeval {
	long	tv_sec;
	long	tv_usec;
};
struct timezone {
	int	tz_minuteswest;
	int	tz_dsttime;
};
struct	itimerval {
	struct	timeval it_interval;
	struct	timeval it_value;
};
typedef	int		sigset_t;
typedef	unsigned int	speed_t;
typedef	unsigned long	tcflag_t;
typedef	unsigned char	cc_t;
typedef	int		pid_t;
typedef	unsigned short	mode_t;
typedef	short		nlink_t;
typedef	long		clock_t;
typedef	long		time_t;
typedef	int		size_t;
typedef int		ptrdiff_t;
typedef	unsigned short	wchar_t;
struct	tm {
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
	int	tm_wday;
	int	tm_yday;
	int	tm_isdst;
	char	*tm_zone;
	long	tm_gmtoff;
};
extern	struct tm *gmtime(), *localtime();
extern	char *asctime(), *ctime();
extern	void tzset(), tzsetwall();
extern  int dysize();
extern  time_t timelocal(), timegm();
struct	rusage {
	struct timeval ru_utime;
	struct timeval ru_stime;
	long	ru_maxrss;
	long	ru_ixrss;
	long	ru_idrss;
	long	ru_isrss;
	long	ru_minflt;
	long	ru_majflt;
	long	ru_nswap;
	long	ru_inblock;
	long	ru_oublock;
	long	ru_msgsnd;
	long	ru_msgrcv;
	long	ru_nsignals;
	long	ru_nvcsw;
	long	ru_nivcsw;
};
struct rlimit {
	int	rlim_cur;
	int	rlim_max;
};
unsigned long
cputime ()
{
    struct rusage rus;
    getrusage (0, &rus);
    return (((rus.ru_utime.tv_sec + rus.ru_stime.tv_sec) * 1000000) +
	    rus.ru_utime.tv_usec + rus.ru_stime.tv_usec) / 1000;
}
int
do_assoc_test (mem, start, cc)
     register char *mem;
     register int start;
     register int cc;
{
  register int j;
  register int ind = start;
  register int x;
  for (j = 0x100000; --j >= 0; )
    {
      int x = ((volatile char *) mem)[ind];
      ind -= cc;
      if (ind < 0)
	ind = start;
    }
  return x;
}
int
determine_assoc (size)
{
  register char *mem = malloc (size * (1 << 5 ));
  int cc = size;
  int lbsets;
  int times[5  + 1];
  for (lbsets = 0; lbsets <= 5; lbsets++)
    {
      int t = cputime ();
      do_assoc_test (mem, (cc << lbsets) - cc, cc);
      t = cputime () - t;
      times[lbsets] = t;
      printf ("sets = %2u: %u ms\n", 1 << lbsets, t);
    }
  free (mem);
  {
    int max = 1000 * times[1] / times[0];
    int maxindex = 0;
    int min = 1000 * times[1] / times[0];
    int minindex = 0;
    for (lbsets = 1; lbsets < 4; lbsets++)
      {
	int x = 1000 * times[lbsets + 1] / times[lbsets];
	if (x > max)
	  {
	    max = x;
	    maxindex = lbsets;
	  }
	if (x < min)
	  {
	    min = x;
	    minindex = lbsets;
	  }
      }
    if (min * 100 > max * 75)
      return 0;
    return 1 << maxindex;
  }
}
do_size_test(mem, size, reps)
     register int *mem;
     register int size;
     register int reps;
{
  register int j, i, x;
  mem += size;
  for (j = reps; --j >= 0;)
    for (i = -size; i < 0; i++)
      x = ((volatile int *) mem)[i];
}
int
determine_size()
{
  register int *mem = malloc (1 << 20 );
  int lbsize;
  int times[20 ];
  for (lbsize = 12; lbsize < 20; lbsize++)
    {
      int t = cputime ();
      do_size_test (mem, (1 << lbsize) / sizeof (int) - 64,
		    0x1000000  >> lbsize);
      t = cputime () - t;
      times[lbsize] = t;
      printf ("size = %2u: %u ms\n", 1 << lbsize, t);
    }
  free (mem);
  {
    int max = 1000 * times[12  + 1] / times[12 ];
    int maxindex = 0;
    int min = 1000 * times[12  + 1] / times[12 ];
    int minindex = 0;
    for (lbsize = 12; lbsize < 20  - 1; lbsize++)
      {
	int x = 1000 * times[lbsize + 1] / times[lbsize];
	if (x > max)
	  {
	    max = x;
	    maxindex = lbsize;
	  }
	if (x < min)
	  {
	    min = x;
	    minindex = lbsize;
	  }
      }
    if (min * 100 > max * 75)
      return 0;
    return 1 << maxindex;
  }
}
int
main()
{
  int size, assoc;
  size = determine_size();
  if (size)
    printf ("I guess this cache is %d bytes.\n", size);
  else
    printf ("I can't determine the size from this run.\n");
  if (size == 0)
    size = 65536;
  assoc = determine_assoc(size);
  if (assoc > 1)
    printf ("I guess this cache is %d-way set associative.\n", assoc);
  else if (assoc == 1)
    printf ("I guess this cache is direct mapped.\n");
  else
    printf ("I can't determine the associativity from this run.\n");
  return 0;
}
