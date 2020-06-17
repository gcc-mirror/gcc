#include <stdlib.h>
typedef struct z_stream_s {
  unsigned char *next_out;
} z_stream;
typedef struct gz_stream {
  z_stream stream;
  unsigned char *outbuf;
} gz_stream;
gz_stream *s;
static void gz_open(const char *path)
{
  s->stream.next_out = s->outbuf = (unsigned char *)malloc(16384); /* { dg-bogus "leak" } */
}
void gzopen(const char *path)
{
  gz_open(path);
}
