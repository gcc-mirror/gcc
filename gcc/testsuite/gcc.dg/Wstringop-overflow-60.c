/* Test derived from Glibc's getifaddrs_internal.   The code could be
   rewritten to avoid the warning for the memcpy call but since unions
   are designed to have their members treated as interchangeable there
   isn't a whole lot to be gained from issuing one.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);

struct sockaddr
{
  short sa_family;
  char sa_data[14];
};

struct in_addr
{
  int s_addr;
};

struct in6_addr
{
  union
  {
    char __u6_addr8[16];
    short __u6_addr16[8];
    int __u6_addr32[4];
  } __in6_u;
};

struct sockaddr_in
{
  short sin_family;
  short sin_port;
  struct in_addr sin_addr;
  unsigned char sin_zero[sizeof (struct sockaddr) -
			 (sizeof (short)) -
			 sizeof (short) -
			 sizeof (struct in_addr)];
};

struct sockaddr_in6
{
  short sin6_family;
  short sin6_port;
  int sin6_flowinfo;
  struct in6_addr sin6_addr;
  int sin6_scope_id;
};

union
{
  struct sockaddr sa;
  struct sockaddr_in s4;
  struct sockaddr_in6 s6;
} u1, u2;

struct sockaddr *sa;

void test_unconditional (void *p)
{
  sa = &u1.sa;
  memcpy (&((struct sockaddr_in6 *) sa)->sin6_addr, p, 16);
}

void test_conditional (void *p, int i)
{
  sa = i ? &u1.sa : &u2.sa;
  memcpy (&((struct sockaddr_in6 *) sa)->sin6_addr, p, 16);
}
