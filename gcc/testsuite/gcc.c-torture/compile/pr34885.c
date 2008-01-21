typedef union {
  __const struct sockaddr *__restrict __sockaddr__;
} __CONST_SOCKADDR_ARG __attribute__ ((__transparent_union__));
extern int _pure_socketcall (const struct sockaddr *);
extern int sendto (__CONST_SOCKADDR_ARG __addr);
int send(void)
{
  return sendto((void *)0);
}
int sendto(const struct sockaddr *to)
{
   return _pure_socketcall(to);
}

