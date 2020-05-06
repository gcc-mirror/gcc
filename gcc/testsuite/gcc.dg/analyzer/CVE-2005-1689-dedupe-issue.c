/* { dg-additional-options "-fanalyzer-show-duplicate-count" } */

#include <stdlib.h>

typedef struct _krb5_data {
  char *data;
} krb5_data;

/* Ensure that we de-duplicate the various paths to reach here,
   and only emit one diagnostic.  */

void
recvauth_common(krb5_data inbuf)
{
  free(inbuf.data);
  free(inbuf.data); /* { dg-warning "double-'free'" "warning" } */
  /* { dg-message "2 duplicates" "duplicates notification" { xfail *-*-* } .-1 } */
}

void krb5_recvauth(krb5_data inbuf)
{
  recvauth_common(inbuf);
}

void krb5_recvauth_version(krb5_data inbuf)
{
  recvauth_common(inbuf);
}
