/* Reduced from git-2.39.0's pack-revindex.c  */

typedef unsigned int __uint32_t;
typedef unsigned long int __uintmax_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef __SIZE_TYPE__ size_t;
typedef __off64_t off_t;
typedef __uint32_t uint32_t;
typedef __uintmax_t uintmax_t;

struct stat {
  /* [...snip...] */
  __off_t st_size;
  /* [...snip...] */
};

extern int close(int __fd);
extern int fstat(int __fd, struct stat *__buf)
  __attribute__((__nothrow__, __leaf__)) __attribute__((__nonnull__(2)));
extern uint32_t default_swab32(uint32_t val);
extern uint32_t git_bswap32(uint32_t x);
__attribute__((__noreturn__)) void die(const char *err, ...)
    __attribute__((format(printf, 1, 2)));
int error(const char *err, ...) __attribute__((format(printf, 1, 2)));
int error_errno(const char *err, ...) __attribute__((format(printf, 1, 2)));
static inline int const_error(void) { return -1; }
extern int munmap(void *__addr, size_t __len)
    __attribute__((__nothrow__, __leaf__));
extern size_t st_mult(size_t a, size_t b);
extern void *xmmap(void *start, size_t length, int prot, int flags, int fd,
		   off_t offset);
extern size_t xsize_t(off_t len);

extern char *gettext(const char *__msgid) __attribute__((__nothrow__, __leaf__))
__attribute__((__format_arg__(1)));
static inline __attribute__((format_arg(1))) const char *_(const char *msgid) {
  if (!*msgid)
    return "";
  return gettext(msgid);
}

struct repository {
  /* [...snip...] */
  const struct git_hash_algo *hash_algo;
  /* [...snip...] */
};
extern struct repository *the_repository;
struct git_hash_algo {
  /* [...snip...] */
  size_t rawsz;
  /* [...snip...] */
};

int git_open_cloexec(const char *name, int flags);

struct revindex_header {
  uint32_t signature;
  uint32_t version;
  uint32_t hash_id;
};

int load_revindex_from_disk(char *revindex_name, uint32_t num_objects,
                            const uint32_t **data_p, size_t *len_p) {
  int fd, ret = 0;
  struct stat st;
  void *data = ((void *)0);
  size_t revindex_size;
  struct revindex_header *hdr;

  fd = git_open_cloexec(revindex_name, 00);

  if (fd < 0) {
    ret = -1;
    goto cleanup;
  }
  if (fstat(fd, &st)) {
    ret = (error_errno(_("failed to read %s"), revindex_name), const_error());
    goto cleanup;
  }

  revindex_size = xsize_t(st.st_size);

  if (revindex_size < ((12) + (2 * the_repository->hash_algo->rawsz))) {
    ret = (error(_("reverse-index file %s is too small"), revindex_name),
           const_error());
    goto cleanup;
  }

  if (revindex_size - ((12) + (2 * the_repository->hash_algo->rawsz)) !=
      st_mult(sizeof(uint32_t), num_objects)) {
    ret = (error(_("reverse-index file %s is corrupt"), revindex_name),
           const_error());
    goto cleanup;
  }

  data = xmmap(((void *)0), revindex_size, 0x1, 0x02, fd, 0);
  hdr = (struct revindex_header *) data;

  if (git_bswap32(hdr->signature) != 0x52494458) {
    ret =
        (error(_("reverse-index file %s has unknown signature"), revindex_name),
         const_error());
    goto cleanup;
  }
  if (git_bswap32(hdr->version) != 1) {
    ret = (error(_("reverse-index file %s has unsupported version %"
                   "u"),
                 revindex_name, git_bswap32(hdr->version)),
           const_error());
    goto cleanup;
  }
  if (!(git_bswap32(hdr->hash_id) == 1 || git_bswap32(hdr->hash_id) == 2)) {
    ret = (error(_("reverse-index file %s has unsupported hash id %"
                   "u"),
                 revindex_name, git_bswap32(hdr->hash_id)),
           const_error());
    goto cleanup;
  }

cleanup:
  if (ret) {
    if (data) /* { dg-bogus "check of 'data' for NULL after already dereferencing it" } */
      munmap(data, revindex_size);
  } else {
    *len_p = revindex_size;
    *data_p = (const uint32_t *)data;
  }

  if (fd >= 0)
    close(fd);
  return ret;
}
