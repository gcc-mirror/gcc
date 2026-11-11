struct Location
{
  const char *file;
  const char *function;
  int line;
};

extern void fail (int, const char *, const Location &)
  __attribute__ ((noreturn, cold, noipa));
extern void note (const char *, int) __attribute__ ((cold, noipa));

static const char source_file[] = __BASE_FILE__;

struct Buffer
{
  int length;

  int printable_length () const
  {
    if (__builtin_expect (length < 1024, 1))
      return length;
    const Location location = { source_file, __func__, __LINE__ };
    note (location.file, location.line);
    note (location.function, length);
    note (location.file, length + 1);
    fail (3, "length < 1024", location);
  }
};
