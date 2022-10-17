typedef unsigned int __u32;
__extension__ typedef __signed__ long long __s64;
__extension__ typedef unsigned long long __u64;
typedef __u32 u32;
typedef __s64 s64;
typedef __u64 u64;
typedef long long __kernel_time64_t;
typedef _Bool bool;
typedef __s64 time64_t;
struct __kernel_timespec {
 __kernel_time64_t tv_sec;
 long long tv_nsec;
};
struct timespec64 {
 time64_t tv_sec;
 long tv_nsec;
};

extern struct timespec64 ns_to_timespec64(const s64 nsec);
int put_timespec64(const struct timespec64 *ts,
  struct __kernel_timespec *uts);

/* [...snip...] */

extern int put_old_timespec32(const struct timespec64 *, void *);

/* [...snip...] */

/* [...snip...] */

typedef s64 ktime_t;

/* [...snip...] */

extern void ktime_get_real_ts64(struct timespec64 *tv);

/* [...snip...] */

enum tk_offsets {
 TK_OFFS_REAL,
 TK_OFFS_BOOT,
 TK_OFFS_TAI,
 TK_OFFS_MAX,
};

extern ktime_t ktime_get(void);
extern ktime_t ktime_get_with_offset(enum tk_offsets offs);
extern ktime_t ktime_get_coarse_with_offset(enum tk_offsets offs);
extern ktime_t ktime_mono_to_any(ktime_t tmono, enum tk_offsets offs);
extern ktime_t ktime_get_raw(void);
extern u32 ktime_get_resolution_ns(void);


static ktime_t ktime_get_real(void)
{
 return ktime_get_with_offset(TK_OFFS_REAL);
}

/* [...snip...] */

struct socket {
 /* [...snip...] */
 struct sock *sk;
 /* [...snip...] */
};

/* [...snip...] */

struct sock {
 /* [...snip...] */
 ktime_t sk_stamp;
 /* [...snip...] */
};

/* [...snip...] */

static ktime_t sock_read_timestamp(struct sock *sk)
{
  return *(const volatile typeof(sk->sk_stamp) *)&(sk->sk_stamp);
}

static void sock_write_timestamp(struct sock *sk, ktime_t kt)
{
  *(volatile typeof(sk->sk_stamp) *)&(sk->sk_stamp) = kt;
}

/* [...snip...] */

int sock_gettstamp(struct socket *sock, void *userstamp,
     bool timeval, bool time32)
{
 struct sock *sk = sock->sk;
 struct timespec64 ts;

 /* [...snip...] */
 ts = ns_to_timespec64((sock_read_timestamp(sk)));
 if (ts.tv_sec == -1)
  return -2;
 if (ts.tv_sec == 0) {
  ktime_t kt = ktime_get_real();
  sock_write_timestamp(sk, kt);
  ts = ns_to_timespec64((kt));
 }

 if (timeval)
  ts.tv_nsec /= 1000;


 if (time32)
  return put_old_timespec32(&ts, userstamp);
 return put_timespec64(&ts, userstamp);
}
