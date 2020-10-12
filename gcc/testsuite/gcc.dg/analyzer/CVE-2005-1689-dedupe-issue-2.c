/* { dg-additional-options "-fanalyzer-show-duplicate-count" } */

#include <stdlib.h>

typedef struct _krb5_data {
  char *data;
} krb5_data;

/* Ensure that we de-duplicate the various paths to reach here,
   and only emit one diagnostic.  */

void
recvauth_common(krb5_data common)
{
  free(common.data);
  free(common.data); /* { dg-warning "double-'free' of 'common.data'" "inner warning" } */
  /* { dg-warning "double-'free' of 'inbuf_a.data' " "inbuf_a warning" { target *-*-* } .-1 } */
  /* { dg-warning "double-'free' of 'inbuf_b.data' " "inbuf_b warning" { target *-*-* } .-2 } */
  /* { dg-message "2 duplicates" "duplicates notification" { xfail *-*-* } .-3 } */
}

void krb5_recvauth(krb5_data inbuf_a)
{
  recvauth_common(inbuf_a);
}

void krb5_recvauth_version(krb5_data inbuf_b)
{
  recvauth_common(inbuf_b);
}
