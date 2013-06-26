/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#define DEBUG_BUFFER_SIZE 80
int unifi_debug = 5;

void
unifi_trace (void* ospriv, int level, const char *fmt, ...)
{
   static char s[DEBUG_BUFFER_SIZE];
   va_list args;
   unsigned int len;

   if (!ospriv)
     return;

   if (unifi_debug >= level)
     {
       va_start (args, fmt);
       len = vsnprintf (&(s)[0], (DEBUG_BUFFER_SIZE), fmt, args);
       va_end (args);

       if (len >= DEBUG_BUFFER_SIZE)
	 {
	   (s)[DEBUG_BUFFER_SIZE - 2] = '\n';
	   (s)[DEBUG_BUFFER_SIZE - 1] = 0;
	 }

       printf ("%s", s);
     }
}

