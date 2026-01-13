/* { dg-do compile } */
/* { dg-options "-std=c11" } */

typedef union {
  const struct sockaddr *_Atomic __sockaddr__;
} __CONST_SOCKADDR_ARG __attribute__((transparent_union));
extern int sendto (__CONST_SOCKADDR_ARG __addr);

int sendto(const struct sockaddr *_Atomic to)
{
	const struct sockaddr * _Atomic *p = &to;
}

