
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
         if (pz_data)
            pz_data = realloc ((void*)pz_data, space_left + space_used + 1 );
         else
            pz_data = malloc (space_left + space_used + 1 );
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


t_bool
is_cxx_header (fname, text)
     tCC *fname;
     tCC *text;
{
  /*  First, check to see if the file is in a C++ directory */
  for (;;)
    {
      switch (*(fname++))
        {
        case 'C': /* check for "CC/" */
          if ((fname[0] == 'C') && (fname[1] == '/'))
            return BOOL_TRUE;
          break;

        case 'x': /* check for "xx/" */
          if ((fname[0] == 'x') && (fname[1] == '/'))
            return BOOL_TRUE;
          break;

        case '+': /* check for "++" */
          if (fname[0] == '+')
            return BOOL_TRUE;
          break;

        case NUL:
          goto not_cxx_name;
        }
    } not_cxx_name:;

  /* Or it might contain the phrase 'extern "C++"' */
  for (;;)
    {
      tSCC zExtern[]   = "extern";
      tSCC zExtCxx[]   = "\"C++\"";
      tSCC zTemplate[] = "template";

      switch (*(text++))
        {
        case 'e':
          /*  Check for "extern \"C++\"" */
          if (strncmp (text, zExtern+1, sizeof( zExtern )-2) != 0)
            break;
          text += sizeof( zExtern )-2;
          if (! isspace( *(text++)) )
            break;
          while (isspace( *text ))  text++;
          if (strncmp (text, zExtCxx, sizeof (zExtCxx) -1) == 0)
            return BOOL_TRUE;
          break;

        case 't':
          /*  Check for "template<" */
          if (strncmp (text, zTemplate+1, sizeof( zTemplate )-2) != 0)
            break;
          text += sizeof( zTemplate )-2;
          while (isspace( *text ))  text++;
          if (*text == '<')
            return BOOL_TRUE;
          break;

        case NUL:
          goto text_done;
          break;
        }
    } text_done:;

  return BOOL_FALSE;
}
