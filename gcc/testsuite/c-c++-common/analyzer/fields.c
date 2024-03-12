typedef __SIZE_TYPE__ size_t;

extern size_t strlen (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__pure__))
  __attribute__ ((__nonnull__ (1)));

extern void *malloc (size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__)) ;

extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

typedef struct _krb5_data {
  unsigned int length;
  char *data;
} krb5_data;

typedef struct _krb5_error {
  krb5_data text;
} krb5_error;

extern const char *error_message (int);

int
recvauth_common (int problem)
{
  if (problem) {
    krb5_error error;
    const char *message = error_message(problem);
    error.text.length = strlen(message) + 1;
    if (!(error.text.data = (char *) malloc(error.text.length))) {
      goto cleanup;
    }
    free(error.text.data);
  }

 cleanup:
  return problem; /* { dg-bogus "leak" } */
}
