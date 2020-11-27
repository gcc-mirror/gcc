// rust-gcc-diagnostics.cc -- GCC implementation of rust diagnostics interface.

#include "rust-system.h"
#include "rust-diagnostics.h"

void
rust_be_error_at(const Location location, const std::string& errmsg)
{
  location_t gcc_loc = location.gcc_location();
  error_at(gcc_loc, "%s", errmsg.c_str());
}

void
rust_be_warning_at(const Location location,
                   int opt, const std::string& warningmsg)
{
  location_t gcc_loc = location.gcc_location();
  warning_at(gcc_loc, opt, "%s", warningmsg.c_str());
}

void
rust_be_fatal_error(const Location location,
                    const std::string& fatalmsg)
{
  location_t gcc_loc = location.gcc_location();
  fatal_error(gcc_loc, "%s", fatalmsg.c_str());
}

void
rust_be_inform(const Location location,
               const std::string& infomsg)
{
  location_t gcc_loc = location.gcc_location();
  inform(gcc_loc, "%s", infomsg.c_str());
}

void
rust_be_get_quotechars(const char** open_qu, const char** close_qu)
{
  *open_qu = open_quote;
  *close_qu = close_quote;
}
