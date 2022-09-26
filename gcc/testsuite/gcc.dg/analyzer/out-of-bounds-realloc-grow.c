/* { dg-additional-options "-Wno-analyzer-too-complex" } */

/* Reduced from gnulib/read-file.c.

   Tests that there is no false-positive on
   realloc when the buffer is growing.  */

#include <stdlib.h>

/* Indicate that the file is treated as binary.  */
#define RF_BINARY 0x1

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

char *
fread_file (FILE *stream, int flags, size_t *length)
{
  char *buf = NULL;
  size_t alloc = BUFSIZ;

  if (!(buf = malloc (alloc)))
    return NULL; /* errno is ENOMEM.  */

  {
    size_t size = 0; /* number of bytes read so far */
    int save_errno;

    for (;;)
      {
        /* This reads 1 more than the size of a regular file
           so that we get eof immediately.  */
        size_t requested = alloc - size;
        size_t count = fread (buf + size, 1, requested, stream);
        size += count;

        {
          char *new_buf;

          if (alloc < PTRDIFF_MAX - alloc / 2)
            alloc = alloc + alloc / 2;
          else
            alloc = PTRDIFF_MAX;

           if (!(new_buf = realloc (buf, alloc)))
            {
              save_errno = errno;
              break;
            }

          buf = new_buf;
        }
      }

    free (buf);
    errno = save_errno;
    return NULL;
  }
}

/* Open and read the contents of FILENAME, and return a newly
   allocated string with the content, and set *LENGTH to the length of
   the string.  The string is zero-terminated, but the terminating
   zero byte is not counted in *LENGTH.  On errors, *LENGTH is
   undefined, errno preserves the values set by system functions (if
   any), and NULL is returned.
   If the RF_BINARY flag is set in FLAGS, the file is opened in binary
   mode.  If the RF_SENSITIVE flag is set in FLAGS, the memory buffer
   internally allocated will be cleared upon failure.  */
char *
read_file (const char *filename, int flags, size_t *length)
{
  const char *mode = (flags & RF_BINARY) ? "rbe" : "re";
  FILE *stream = fopen (filename, mode);
  char *out;

  if (!stream)
    return NULL;

  out = fread_file (stream, flags, length);

  fclose (stream);

  return out;
}
