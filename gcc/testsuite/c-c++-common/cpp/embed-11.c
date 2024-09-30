/* { dg-do run { target native } } */
/* { dg-options "-DMAGNA_CARTA=\"${srcdir}/c-c++-common/cpp/embed-dir/magna-carta.txt\"" } */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int
main (void)
{
  static const unsigned char embed_data[] = {
    #embed MAGNA_CARTA
  };
  char f_data[sizeof (embed_data)];
  FILE *f_source = fopen (MAGNA_CARTA, "rb");
  if (f_source == NULL)
    abort ();
  if (fread (f_data, 1, sizeof (embed_data),
	     f_source) != sizeof (embed_data))
    {
      fclose (f_source);
      abort ();
    }
  fclose (f_source);
  if (memcmp (&embed_data[0], f_data, sizeof (embed_data)))
    abort ();
}
