/* Copyright 2000 Free Software Foundation

   by Alexandre Oliva  <aoliva@redhat.com>

   Based on zlib/gzio.c.

   This used to generate duplicate labels when compiled with
   sh-elf-gcc -O2 -m3 -fPIC.

   Bug reported by NIIBE Yutaka <gniibe@m17n.org>.  */

void foo (void);

void
bar ()
{
    unsigned len;

    for (len = 0; len < 2; len++)
	foo ();
}
