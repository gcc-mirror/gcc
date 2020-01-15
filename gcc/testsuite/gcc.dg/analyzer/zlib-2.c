/* { dg-additional-options "-Wno-analyzer-too-complex" } */

typedef void * (*alloc_func)(void * opaque, unsigned items, unsigned size);
typedef void (*free_func)(void * opaque, void * address);

typedef struct z_stream_s {
  char *msg;
  alloc_func zalloc;
  free_func zfree;
  void * opaque;
} z_stream;

void * zcalloc(void * opaque, unsigned items, unsigned size);
void zcfree(void * opaque, void * ptr);

int deflateInit2_(z_stream *strm, int level, int method, int windowBits,
                  int memLevel, int strategy, const char *version,
                  int stream_size) {
  int noheader = 0;
  static const char *my_version = "1.1.3";

  if (version == 0 || version[0] != my_version[0] ||
      stream_size != sizeof(z_stream)) {
    return (-6);
  }
  if (strm == 0)
    return (-2);

  strm->msg = 0;
  if (strm->zalloc == 0) {
    strm->zalloc = zcalloc;
    strm->opaque = (void *)0;
  }
  if (strm->zfree == 0)
    strm->zfree = zcfree;

  if (level == (-1))
    level = 6;

  if (windowBits < 0) {
    noheader = 1;
    windowBits = -windowBits;
  }
  if (memLevel < 1 || memLevel > 9 || method != 8 || windowBits < 8 ||
      windowBits > 15 || level < 0 || level > 9 || strategy < 0 ||
      strategy > 2) {
    return (-2);
  }
  (*((strm)->zalloc))((strm)->opaque, (1), 112);
  return 0;
}
