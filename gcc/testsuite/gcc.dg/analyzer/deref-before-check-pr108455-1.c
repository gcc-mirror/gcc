extern int could_fail_1 (void);
extern void *could_fail_2 (int);
extern void cleanup (void *);

struct header {
  int signature;
};

int test_1 (void) {
  int fd, ret = 0;
  void *data = ((void *)0);
  struct header *hdr;

  fd = could_fail_1 ();

  if (fd < 0) {
    ret = -1;
    goto cleanup;
  }

  data = could_fail_2 (fd);
  hdr = data;

  if (hdr->signature != 42) {
    ret = -2;
    goto cleanup;
  }

cleanup:
  if (ret) {
    if (data) /* { dg-bogus "check of 'data' for NULL after already dereferencing it" } */
      cleanup (data);
  }

  return ret;
}
