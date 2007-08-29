// PR c++/33194
void c_translate_location (
           void (*fail) (
           const char *fmt, ...)
           __attribute__ ((noreturn,
             format (printf, 1, 2)))
           );


struct dwflpp
{
  static void loc2c_error (const char *fmt, ...)
  {
  }

  void
  translate_location()
  {
    return c_translate_location (&loc2c_error);
  }
};
