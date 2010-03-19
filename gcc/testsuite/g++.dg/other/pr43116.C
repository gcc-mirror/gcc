/* { dg-do compile } */
extern "C" int rpl_open (const char *filename, int flags, ...) __attribute__
((__nonnull__ (1)));

namespace gnulib
{
    int (*const open) (const char *filename, int flags, ...) __attribute__
	((__nonnull__ (1))) = rpl_open;
}
