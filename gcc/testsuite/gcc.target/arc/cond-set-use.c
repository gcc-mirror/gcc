/* { dg-do run } */
/* { dg-options "-Os" } */

/* Based on gethostbyname_r,
 * Copyright (C) 2000-2006 Erik Andersen <andersen@uclibc.org>
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB
 *
 * Extraction / wrapping as test by
 * Joern Rennecke  <joern.rennecke@embecosm.com>
 * Copyright (C) 2013 Free Software Foundation, Inc.
 */

typedef unsigned size_t;
typedef int ssize_t;
typedef unsigned uint32_t;
struct resolv_answer {
 char *dotted;
 int atype;
 int aclass;
 int ttl;
 int rdlength;
 const unsigned char *rdata;
 int rdoffset;
 char* buf;
 size_t buflen;
 size_t add_count;
};
struct hostent
{
  char *h_name;
  char **h_aliases;
  int h_addrtype;
  int h_length;
  char **h_addr_list;
};

int *__attribute__  ((noinline,weak)) nop (void * p) { return p; };
void __attribute__  ((noinline,weak)) seta (struct resolv_answer * p)
{ p->atype = 1;}

int ghostbyname_r(
  struct hostent *result_buf,
  char *buf,
  size_t buflen,
  struct hostent **result,
  int *h_errnop)
{
 char **addr_list;
 char **alias;
 char *alias0;
 int i0;
 struct resolv_answer a;
 int i;

 *result = ((void *)0);

         *h_errnop = -1;

         if ((ssize_t)buflen <= 5)
  return 34;

 alias = (char **)buf;
 addr_list = (char **)buf;

  /* This got turned into branch with conditional move in delay slot.  */
 if ((ssize_t)buflen < 256)
  return 34;


 {
  if (!nop(&i0)) {
   result_buf->h_aliases = alias;
   result_buf->h_addrtype = 2;
   result_buf->h_length = 4;
   result_buf->h_addr_list = addr_list;
   *result = result_buf;
   *h_errnop = 0;
   return 0;
  }
 }


 seta (&a);

 if (a.atype == 1) {

  int need_bytes = sizeof(addr_list[0]) * (a.add_count + 1 + 1);

  int ips_len = a.add_count * a.rdlength;

  buflen -= (need_bytes + ips_len);
  if ((ssize_t)buflen < 0) {
   i = 34;
   goto free_and_ret;
  }

  result_buf->h_addrtype = 2;
  *result = result_buf;
  *h_errnop = 0;
  i = 0;
  goto free_and_ret;
 }

 /* For cse, the 1 was is loaded into a call-saved register;
    the load was hoisted into a delay slot before the conditional load,
    clobbering result_buf, which (conditionally) lived in the same
    call-saved register, because mark_referenced_resources considered the
    destination of the COND_EXEC only clobbered, but not used.  */
 *h_errnop = 1;
 *nop(&i0) = 1;
 i = 2;

 free_and_ret:
 nop (&i0);
 return i;
}

int
main ()
{
  struct hostent buf, *res;
  int i;
  char c;
  ghostbyname_r (&buf,  &c, 1024, &res, &i);
  ghostbyname_r (&buf,  0, 1024, &res, &i);
  return 0;
}
