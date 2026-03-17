// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for string/memory" { ! hostedlib } }
// PR tree-optimization/120987

#include <memory>
#include <string>
#include <cstdlib>

#define ERROR_STRING  "012345678901234567"

struct gdb_exception
{
  gdb_exception (const char *s)
    : message (std::make_shared<std::string> (s))
  {}

  explicit gdb_exception (gdb_exception &&other) noexcept
    : message (std::move (other.message))
  {
    volatile int a = 1;
    if (a != 1)
      abort ();
  }
  

  std::shared_ptr<std::string> message;
};

void __attribute__((noinline, noclone))
throw_exception (gdb_exception &&exception)
{
  throw gdb_exception (std::move (exception));
}

static void __attribute__((noinline, noclone))
parse_linespec (void)
{
  struct gdb_exception file_exception (ERROR_STRING);
  throw_exception (std::move (file_exception));
}

int
main (void)
{
  try
    {
      parse_linespec ();
    }
  catch (const gdb_exception &e)
    {
      if (*e.message != ERROR_STRING)
        __builtin_abort();
      return 0;
    }

  __builtin_abort();
}
