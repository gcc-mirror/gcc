
#include "fixlib.h"

/* * * * * * * * * * * * *
 
   load_file_data loads all the contents of a file into malloc-ed memory.
   Its argument is the file pointer of the file to read in; the returned
   result is the NUL terminated contents of the file.  The file
   is presumed to be an ASCII text file containing no NULs.  */

char *
load_file_data (fp)
     FILE* fp;
{
  char *pz_data = (char*)NULL;
  int    space_left = -1;  /* allow for terminating NUL */
  size_t space_used = 0;

  do
    {
      size_t  size_read;

      if (space_left < 1024)
        {
          space_left += 4096;
          pz_data = realloc ((void*)pz_data, space_left + space_used + 1 );
        }
      size_read = fread (pz_data + space_used, 1, space_left, fp);

      if (size_read == 0)
        {
          if (feof (fp))
            break;

          if (ferror (fp))
            {
              int err = errno;
              if (err != EISDIR)
                fprintf (stderr, "error %d (%s) reading input\n", err,
                         strerror (err));
              free ((void *) pz_data);
              fclose (fp);
              return (char *) NULL;
            }
        }

      space_left -= size_read;
      space_used += size_read;
    } while (! feof (fp));

  pz_data = realloc ((void*)pz_data, space_used+1 );
  pz_data[ space_used ] = NUL;
  fclose (fp);

  return pz_data;
}
