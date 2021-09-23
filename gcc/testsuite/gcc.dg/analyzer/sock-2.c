__extension__ typedef __signed__ long long __s64;
typedef __s64 time64_t;
struct timespec64 {
 time64_t tv_sec;
 long tv_nsec;
};

extern struct timespec64 ns_to_timespec64(void);

int sock_gettstamp()
{
 struct timespec64 ts;

 /* [...snip...] */
 ts = ns_to_timespec64();
 if (ts.tv_sec == -1)
  return -2;
 /* [...snip...] */
 return 0;
}
