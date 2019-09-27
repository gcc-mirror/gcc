typedef long unsigned int size_t;
typedef struct _IO_FILE FILE;
extern size_t fread(void *__restrict __ptr, size_t __size, size_t __n,
                    FILE *__restrict __stream);
typedef unsigned char Byte;
typedef unsigned int uInt;
typedef unsigned long uLong;

typedef struct gz_stream {
  FILE *file;
  uInt avail_in;
  uInt avail_out;
} gz_stream;

void test_1_callee(gz_stream *s, Byte *buf) {
  Byte *next_out = buf;
  uInt n = s->avail_in;
  if (n > 0) {
    next_out += n;
  }
  s->avail_out -= fread(next_out, 1, s->avail_out, s->file);
}

void test_1_caller(gz_stream *s) {
  unsigned char c;
  test_1_callee(s, &c);
}
