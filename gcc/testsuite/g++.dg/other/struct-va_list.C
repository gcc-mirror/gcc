// { dg-do compile }
// PR target/13302

#include <stdarg.h>

struct NumArgState{
    va_list ap;
};
