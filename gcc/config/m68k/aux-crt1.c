/* Startup code for A/UX
   Copyright (C) 1996 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* This file is compiled three times to produce crt1.o, mcrt1.o, and
   maccrt1.o.  The final two are created by defining MCRT1 and MACCRT1
   respectively.  */

#include <stdlib.h>
#ifdef MCRT1
#include <unistd.h>
#include <mon.h>
#endif

/* Extern function declarations */

extern void initfpu(void);
extern void __istart(void);
extern void __compatmode(void);
extern void _cleanup(void);
extern int main(int, char **, char **);
extern void exit(int) __attribute__((noreturn));
extern void _exit(int) __attribute__((noreturn));

#ifdef MACCRT1
extern void InitMac(void);
#endif
#ifdef MCRT1
static void monitor_start(void);
#endif

/* Global variables */

char **environ;
char *__splimit;			/* address of top of stack */


/* Initialize system and run */

void _start() __attribute__((noreturn));
void _start()
{
  register int *fp __asm__("%a6");
  register char *d0 __asm__("%d0");
  char **argv;
  int argc;

  __splimit = d0;
  argc = fp[1];
  argv = (char **)&fp[2];
  environ = &argv[argc+1];

  initfpu();
  __istart();
  __compatmode();

  atexit(_cleanup);
#ifdef MCRT1
  monitor_start();
#endif
#ifdef MACCRT1
  InitMac();
#endif

  exit(main(argc, argv, environ));
}


#ifdef MCRT1
/* Start/Stop program monitor */

extern void monitor(void *, void *, WORD *, int, int);

static WORD *monitor_buffer;

static void monitor_cleanup(void)
{
  monitor(NULL, NULL, NULL, 0, 0);
  free(monitor_buffer);
}

static void monitor_start(void)
{
  extern int etext;
  extern int stext __asm__(".text");

  /* Choice of buffer size should be "no more than a few times
     smaller than the program size" -- I don't believe that there
     are any (useful) functions smaller than two insns (4 bytes)
     so that is the scale factor used here */
  int len = (&etext - &stext + 1) / 4;

  monitor_buffer = (WORD *)calloc(len, sizeof(WORD));
  if (monitor_buffer == NULL)
    {
      static const char msg[] = "mcrt1: could not allocate monitor buffer\n";
      write(2, msg, sizeof(msg)-1);
      _exit(-1);
    }

  /* I'm not sure why the count cap at 600 -- but that is what A/UX does */
  monitor(&stext, &etext, monitor_buffer, len, 600);

  atexit(monitor_cleanup);
}
#endif /* MCRT1 */
