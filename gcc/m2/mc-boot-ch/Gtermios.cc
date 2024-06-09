/* Gtermios.cc handwritten module for mc.

Copyright (C) 2010-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"

#include "gm2-libs-host.h"

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef TERMIOS_NEEDS_XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#if defined(__cplusplus)
#define EXTERN extern "C"
#else
#define EXTERN
#endif

#define EXPORT(X) termios##_##X

typedef enum {
  vintr,
  vquit,
  verase,
  vkill,
  veof,
  vtime,
  vmin,
  vswtc,
  vstart,
  vstop,
  vsusp,
  veol,
  vreprint,
  vdiscard,
  vwerase,
  vlnext,
  veol2
} ControlChar;

typedef enum {
  /* input flag bits.  */
  ignbrk,
  ibrkint,
  ignpar,
  iparmrk,
  inpck,
  istrip,
  inlcr,
  igncr,
  icrnl,
  iuclc,
  ixon,
  ixany,
  ixoff,
  imaxbel,
  /* output flag bits.  */
  opost,
  olcuc,
  onlcr,
  ocrnl,
  onocr,
  onlret,
  ofill,
  ofdel,
  onl0,
  onl1,
  ocr0,
  ocr1,
  ocr2,
  ocr3,
  otab0,
  otab1,
  otab2,
  otab3,
  obs0,
  obs1,
  off0,
  off1,
  ovt0,
  ovt1,
  /* baud rate.  */
  b0,
  b50,
  b75,
  b110,
  b135,
  b150,
  b200,
  b300,
  b600,
  b1200,
  b1800,
  b2400,
  b4800,
  b9600,
  b19200,
  b38400,
  b57600,
  b115200,
  b240400,
  b460800,
  b500000,
  b576000,
  b921600,
  b1000000,
  b1152000,
  b1500000,
  b2000000,
  b2500000,
  b3000000,
  b3500000,
  b4000000,
  maxbaud,
  crtscts,
  /* character size.  */
  cs5,
  cs6,
  cs7,
  cs8,
  cstopb,
  cread,
  parenb,
  parodd,
  hupcl,
  clocal,
  /* local flags.  */
  lisig,
  licanon,
  lxcase,
  lecho,
  lechoe,
  lechok,
  lechonl,
  lnoflsh,
  ltopstop,
  lechoctl,
  lechoprt,
  lechoke,
  lflusho,
  lpendin,
  liexten
} Flag;

int
doSetUnset (tcflag_t *bitset, unsigned int mask, int value)
{
  if (value)
    (*bitset) |= mask;
  else
    (*bitset) &= (~mask);
  return 1;
}


#ifdef __cplusplus
extern "C" {
#endif

/* InitTermios - new data structure.  */

void *
EXPORT (InitTermios) (void)
{
  struct termios *p = (struct termios *)malloc (sizeof (struct termios));

  memset (p, 0, sizeof (struct termios));
  return p;
}

/* KillTermios - delete data structure.  */

void *
EXPORT (KillTermios) (struct termios *p)
{
  free (p);
  return NULL;
}

/* tcsnow - return the value of TCSANOW.  */

int
EXPORT (tcsnow) (void)
{
  return TCSANOW;
}

/* tcsdrain - return the value of TCSADRAIN.  */

int
EXPORT (tcsdrain) (void)
{
  return TCSADRAIN;
}

/* tcsflush - return the value of TCSAFLUSH.  */

int
EXPORT (tcsflush) (void)
{
  return TCSAFLUSH;
}

/* cfgetospeed - return output baud rate.  */

int
EXPORT (cfgetospeed) (struct termios *t)
{
  return cfgetospeed (t);
}

/* cfgetispeed - return input baud rate.  */

int
EXPORT (cfgetispeed) (struct termios *t)
{
  return cfgetispeed (t);
}

/* cfsetospeed - set output baud rate.  */

int
EXPORT (cfsetospeed) (struct termios *t, unsigned int b)
{
  return cfsetospeed (t, b);
}

/* cfsetispeed - set input baud rate.  */

int
EXPORT (cfsetispeed) (struct termios *t, unsigned int b)
{
  return cfsetispeed (t, b);
}

/* cfsetspeed - set input and output baud rate.  */

int
EXPORT (cfsetspeed) (struct termios *t, unsigned int b)
{
  int val = cfsetispeed (t, b);
  if (val == 0)
    return cfsetospeed (t, b);
  cfsetospeed (t, b);
  return val;
}

/* tcgetattr - get state of, fd, into, t.  */

int
EXPORT (tcgetattr) (int fd, struct termios *t)
{
  return tcgetattr (fd, t);
}

/* tcsetattr - set state of, fd, to, t, using option.  */

int
EXPORT (tcsetattr) (int fd, int option, struct termios *t)
{
  return tcsetattr (fd, option, t);
}

/* cfmakeraw - sets the terminal to raw mode.  */

void
EXPORT (cfmakeraw) (struct termios *t)
{
#if defined(HAVE_CFMAKERAW)
  return cfmakeraw (t);
#endif
}

/* tcsendbreak - send zero bits for duration.  */

int
EXPORT (tcsendbreak) (int fd, int duration)
{
  return tcsendbreak (fd, duration);
}

/* tcdrain - waits for pending output to be written on, fd.  */

int
EXPORT (tcdrain) (int fd)
{
  return tcdrain (fd);
}

/* tcflushi - flush input.  */

int
EXPORT (tcflushi) (int fd)
{
#if defined(TCIFLUSH)
  return tcflush (fd, TCIFLUSH);
#else
  return 1;
#endif
}

/* tcflusho - flush output.  */

int
EXPORT (tcflusho) (int fd)
{
#if defined(TCOFLUSH)
  return tcflush (fd, TCOFLUSH);
#else
  return 1;
#endif
}

/* tcflushio - flush input and output.  */

int
EXPORT (tcflushio) (int fd)
{
#if defined(TCIOFLUSH)
  return tcflush (fd, TCIOFLUSH);
#else
  return 1;
#endif
}

/* tcflowoni - restart input on, fd.  */

int
EXPORT (tcflowoni) (int fd)
{
#if defined(TCION)
  return tcflow (fd, TCION);
#else
  return 1;
#endif
}

/* tcflowoffi - stop input on, fd.  */

int
EXPORT (tcflowoffi) (int fd)
{
#if defined(TCIOFF)
  return tcflow (fd, TCIOFF);
#else
  return 1;
#endif
}

/* tcflowono - restart output on, fd.  */

int
EXPORT (tcflowono) (int fd)
{
#if defined(TCOON)
  return tcflow (fd, TCOON);
#else
  return 1;
#endif
}

/* tcflowoffo - stop output on, fd.  */

int
EXPORT (tcflowoffo) (int fd)
{
#if defined(TCOOFF)
  return tcflow (fd, TCOOFF);
#else
  return 1;
#endif
}

/* GetFlag - sets a flag value from, t, in, b, and returns TRUE if,
   t, supports, f.  */

int
EXPORT (GetFlag) (struct termios *t, Flag f, int *b)
{
  switch (f)
    {

    case ignbrk:
#if defined(IGNBRK)
      *b = ((t->c_iflag & IGNBRK) == IGNBRK);
      return 1;
#else
      return 0;
#endif
    case ibrkint:
#if defined(BRKINT)
      *b = ((t->c_iflag & BRKINT) == BRKINT);
      return 1;
#else
      return 0;
#endif
    case ignpar:
#if defined(IGNPAR)
      *b = ((t->c_iflag & IGNPAR) == IGNPAR);
      return 1;
#else
      return 0;
#endif
    case iparmrk:
#if defined(PARMRK)
      *b = ((t->c_iflag & PARMRK) == PARMRK);
      return 1;
#else
      return 0;
#endif
    case inpck:
#if defined(INPCK)
      *b = ((t->c_iflag & INPCK) == INPCK);
      return 1;
#else
      return 0;
#endif
    case istrip:
#if defined(ISTRIP)
      *b = ((t->c_iflag & ISTRIP) == ISTRIP);
      return 1;
#else
      return 0;
#endif
    case inlcr:
#if defined(INLCR)
      *b = ((t->c_iflag & INLCR) == INLCR);
      return 1;
#else
      return 0;
#endif
    case igncr:
#if defined(IGNCR)
      *b = ((t->c_iflag & IGNCR) == IGNCR);
      return 1;
#else
      return 0;
#endif
    case icrnl:
#if defined(ICRNL)
      *b = ((t->c_iflag & ICRNL) == ICRNL);
      return 1;
#else
      return 0;
#endif
    case iuclc:
#if defined(IUCLC)
      *b = ((t->c_iflag & IUCLC) == IUCLC);
      return 1;
#else
      return 0;
#endif
    case ixon:
#if defined(IXON)
      *b = ((t->c_iflag & IXON) == IXON);
      return 1;
#else
      return 0;
#endif
    case ixany:
#if defined(IXANY)
      *b = ((t->c_iflag & IXANY) == IXANY);
      return 1;
#else
      return 0;
#endif
    case ixoff:
#if defined(IXOFF)
      *b = ((t->c_iflag & IXOFF) == IXOFF);
      return 1;
#else
      return 0;
#endif
    case imaxbel:
#if defined(IMAXBEL)
      *b = ((t->c_iflag & IMAXBEL) == IMAXBEL);
      return 1;
#else
      return 0;
#endif
    case opost:
#if defined(OPOST)
      *b = ((t->c_oflag & OPOST) == OPOST);
      return 1;
#else
      return 0;
#endif
    case olcuc:
#if defined(OLCUC)
      *b = ((t->c_oflag & OLCUC) == OLCUC);
      return 1;
#else
      return 0;
#endif
    case onlcr:
#if defined(ONLCR)
      *b = ((t->c_oflag & ONLCR) == ONLCR);
      return 1;
#else
      return 0;
#endif
    case ocrnl:
#if defined(OCRNL)
      *b = ((t->c_oflag & OCRNL) == OCRNL);
      return 1;
#else
      return 0;
#endif
    case onocr:
#if defined(ONOCR)
      *b = ((t->c_oflag & ONOCR) == ONOCR);
      return 1;
#else
      return 0;
#endif
    case onlret:
#if defined(ONLRET)
      *b = ((t->c_oflag & ONLRET) == ONLRET);
      return 1;
#else
      return 0;
#endif
    case ofill:
#if defined(OFILL)
      *b = ((t->c_oflag & OFILL) == OFILL);
      return 1;
#else
      return 0;
#endif
    case ofdel:
#if defined(OFDEL)
      *b = ((t->c_oflag & OFDEL) == OFDEL);
      return 1;
#else
      return 0;
#endif
    case onl0:
#if defined(NL0)
      *b = ((t->c_oflag & NL0) == NL0);
      return 1;
#else
      return 0;
#endif
    case onl1:
#if defined(NL1)
      *b = ((t->c_oflag & NL1) == NL1);
      return 1;
#else
      return 0;
#endif
    case ocr0:
#if defined(CR0)
      *b = ((t->c_oflag & CR0) == CR0);
      return 1;
#else
      return 0;
#endif
    case ocr1:
#if defined(CR1)
      *b = ((t->c_oflag & CR1) == CR1);
      return 1;
#else
      return 0;
#endif
    case ocr2:
#if defined(CR2)
      *b = ((t->c_oflag & CR2) == CR2);
      return 1;
#else
      return 0;
#endif
    case ocr3:
#if defined(CR3)
      *b = ((t->c_oflag & CR3) == CR3);
      return 1;
#else
      return 0;
#endif
    case otab0:
#if defined(TAB0)
      *b = ((t->c_oflag & TAB0) == TAB0);
      return 1;
#else
      return 0;
#endif
    case otab1:
#if defined(TAB1)
      *b = ((t->c_oflag & TAB1) == TAB1);
      return 1;
#else
      return 0;
#endif
    case otab2:
#if defined(TAB2)
      *b = ((t->c_oflag & TAB2) == TAB2);
      return 1;
#else
      return 0;
#endif
    case otab3:
#if defined(TAB3)
      *b = ((t->c_oflag & TAB3) == TAB3);
      return 1;
#else
      return 0;
#endif
    case obs0:
#if defined(BS0)
      *b = ((t->c_oflag & BS0) == BS0);
      return 1;
#else
      return 0;
#endif
    case obs1:
#if defined(BS1)
      *b = ((t->c_oflag & BS1) == BS1);
      return 1;
#else
      return 0;
#endif
    case off0:
#if defined(FF0)
      *b = ((t->c_oflag & FF0) == FF0);
      return 1;
#else
      return 0;
#endif
    case off1:
#if defined(FF1)
      *b = ((t->c_oflag & FF1) == FF1);
      return 1;
#else
      return 0;
#endif
    case ovt0:
#if defined(VT0)
      *b = ((t->c_oflag & VT0) == VT0);
      return 1;
#else
      return 0;
#endif
    case ovt1:
#if defined(VT1)
      *b = ((t->c_oflag & VT1) == VT1);
      return 1;
#else
      return 0;
#endif
    case b0:
#if defined(B0)
      *b = ((t->c_cflag & B0) == B0);
      return 1;
#else
      return 0;
#endif
    case b50:
#if defined(B50)
      *b = ((t->c_cflag & B50) == B50);
      return 1;
#else
      return 0;
#endif
    case b75:
#if defined(B75)
      *b = ((t->c_cflag & B75) == B75);
      return 1;
#else
      return 0;
#endif
    case b110:
#if defined(B110)
      *b = ((t->c_cflag & B110) == B110);
      return 1;
#else
      return 0;
#endif
    case b135:
#if defined(B134)
      *b = ((t->c_cflag & B134) == B134);
      return 1;
#else
      return 0;
#endif
    case b150:
#if defined(B150)
      *b = ((t->c_cflag & B150) == B150);
      return 1;
#else
      return 0;
#endif
    case b200:
#if defined(B200)
      *b = ((t->c_cflag & B200) == B200);
      return 1;
#else
      return 0;
#endif
    case b300:
#if defined(B300)
      *b = ((t->c_cflag & B300) == B300);
      return 1;
#else
      return 0;
#endif
    case b600:
#if defined(B600)
      *b = ((t->c_cflag & B600) == B600);
      return 1;
#else
      return 0;
#endif
    case b1200:
#if defined(B1200)
      *b = ((t->c_cflag & B1200) == B1200);
      return 1;
#else
      return 0;
#endif
    case b1800:
#if defined(B1800)
      *b = ((t->c_cflag & B1800) == B1800);
      return 1;
#else
      return 0;
#endif
    case b2400:
#if defined(B2400)
      *b = ((t->c_cflag & B2400) == B2400);
      return 1;
#else
      return 0;
#endif
    case b4800:
#if defined(B4800)
      *b = ((t->c_cflag & B4800) == B4800);
      return 1;
#else
      return 0;
#endif
    case b9600:
#if defined(B9600)
      *b = ((t->c_cflag & B9600) == B9600);
      return 1;
#else
      return 0;
#endif
    case b19200:
#if defined(B19200)
      *b = ((t->c_cflag & B19200) == B19200);
      return 1;
#else
      return 0;
#endif
    case b38400:
#if defined(B38400)
      *b = ((t->c_cflag & B38400) == B38400);
      return 1;
#else
      return 0;
#endif
    case b57600:
#if defined(B57600)
      *b = ((t->c_cflag & B57600) == B57600);
      return 1;
#else
      return 0;
#endif
    case b115200:
#if defined(B115200)
      *b = ((t->c_cflag & B115200) == B115200);
      return 1;
#else
      return 0;
#endif
    case b240400:
#if defined(B230400)
      *b = ((t->c_cflag & B230400) == B230400);
      return 1;
#else
      return 0;
#endif
    case b460800:
#if defined(B460800)
      *b = ((t->c_cflag & B460800) == B460800);
      return 1;
#else
      return 0;
#endif
    case b500000:
#if defined(B500000)
      *b = ((t->c_cflag & B500000) == B500000);
      return 1;
#else
      return 0;
#endif
    case b576000:
#if defined(B576000)
      *b = ((t->c_cflag & B576000) == B576000);
      return 1;
#else
      return 0;
#endif
    case b921600:
#if defined(B921600)
      *b = ((t->c_cflag & B921600) == B921600);
      return 1;
#else
      return 0;
#endif
    case b1000000:
#if defined(B1000000)
      *b = ((t->c_cflag & B1000000) == B1000000);
      return 1;
#else
      return 0;
#endif
    case b1152000:
#if defined(B1152000)
      *b = ((t->c_cflag & B1152000) == B1152000);
      return 1;
#else
      return 0;
#endif
    case b1500000:
#if defined(B1500000)
      *b = ((t->c_cflag & B1500000) == B1500000);
      return 1;
#else
      return 0;
#endif
    case b2000000:
#if defined(B2000000)
      *b = ((t->c_cflag & B2000000) == B2000000);
      return 1;
#else
      return 0;
#endif
    case b2500000:
#if defined(B2500000)
      *b = ((t->c_cflag & B2500000) == B2500000);
      return 1;
#else
      return 0;
#endif
    case b3000000:
#if defined(B3000000)
      *b = ((t->c_cflag & B3000000) == B3000000);
      return 1;
#else
      return 0;
#endif
    case b3500000:
#if defined(B3500000)
      *b = ((t->c_cflag & B3500000) == B3500000);
      return 1;
#else
      return 0;
#endif
    case b4000000:
#if defined(B4000000)
      *b = ((t->c_cflag & B4000000) == B4000000);
      return 1;
#else
      return 0;
#endif
    case maxbaud:
#if defined(__MAX_BAUD)
      *b = ((t->c_cflag & __MAX_BAUD) == __MAX_BAUD);
      return 1;
#else
      return 0;
#endif
    case crtscts:
#if defined(CRTSCTS)
      *b = ((t->c_cflag & CRTSCTS) == CRTSCTS);
      return 1;
#else
      return 0;
#endif
    case cs5:
#if defined(CS5)
      *b = ((t->c_cflag & CS5) == CS5);
      return 1;
#else
      return 0;
#endif
    case cs6:
#if defined(CS6)
      *b = ((t->c_cflag & CS6) == CS6);
      return 1;
#else
      return 0;
#endif
    case cs7:
#if defined(CS7)
      *b = ((t->c_cflag & CS7) == CS7);
      return 1;
#else
      return 0;
#endif
    case cs8:
#if defined(CS8)
      *b = ((t->c_cflag & CS8) == CS8);
      return 1;
#else
      return 0;
#endif
    case cstopb:
#if defined(CSTOPB)
      *b = ((t->c_cflag & CSTOPB) == CSTOPB);
      return 1;
#else
      return 0;
#endif
    case cread:
#if defined(CREAD)
      *b = ((t->c_cflag & CREAD) == CREAD);
      return 1;
#else
      return 0;
#endif
    case parenb:
#if defined(PARENB)
      *b = ((t->c_cflag & PARENB) == PARENB);
      return 1;
#else
      return 0;
#endif
    case parodd:
#if defined(PARODD)
      *b = ((t->c_cflag & PARODD) == PARODD);
      return 1;
#else
      return 0;
#endif
    case hupcl:
#if defined(HUPCL)
      *b = ((t->c_cflag & HUPCL) == HUPCL);
      return 1;
#else
      return 0;
#endif
    case clocal:
#if defined(CLOCAL)
      *b = ((t->c_cflag & CLOCAL) == CLOCAL);
      return 1;
#else
      return 0;
#endif
    case lisig:
#if defined(ISIG)
      *b = ((t->c_lflag & ISIG) == ISIG);
      return 1;
#else
      return 0;
#endif
    case licanon:
#if defined(ICANON)
      *b = ((t->c_lflag & ICANON) == ICANON);
      return 1;
#else
      return 0;
#endif
    case lxcase:
#if defined(XCASE)
      *b = ((t->c_lflag & XCASE) == XCASE);
      return 1;
#else
      return 0;
#endif
    case lecho:
#if defined(ECHO)
      *b = ((t->c_lflag & ECHO) == ECHO);
      return 1;
#else
      return 0;
#endif
    case lechoe:
#if defined(ECHOE)
      *b = ((t->c_lflag & ECHOE) == ECHOE);
      return 1;
#else
      return 0;
#endif
    case lechok:
#if defined(ECHOK)
      *b = ((t->c_lflag & ECHOK) == ECHOK);
      return 1;
#else
      return 0;
#endif
    case lechonl:
#if defined(ECHONL)
      *b = ((t->c_lflag & ECHONL) == ECHONL);
      return 1;
#else
      return 0;
#endif
    case lnoflsh:
#if defined(NOFLSH)
      *b = ((t->c_lflag & NOFLSH) == NOFLSH);
      return 1;
#else
      return 0;
#endif
    case ltopstop:
#if defined(TOSTOP)
      *b = ((t->c_lflag & TOSTOP) == TOSTOP);
      return 1;
#else
      return 0;
#endif
    case lechoctl:
#if defined(ECHOCTL)
      *b = ((t->c_lflag & ECHOCTL) == ECHOCTL);
      return 1;
#else
      return 0;
#endif
    case lechoprt:
#if defined(ECHOPRT)
      *b = ((t->c_lflag & ECHOPRT) == ECHOPRT);
      return 1;
#else
      return 0;
#endif
    case lechoke:
#if defined(ECHOKE)
      *b = ((t->c_lflag & ECHOKE) == ECHOKE);
      return 1;
#else
      return 0;
#endif
    case lflusho:
#if defined(FLUSHO)
      *b = ((t->c_lflag & FLUSHO) == FLUSHO);
      return 1;
#else
      return 0;
#endif
    case lpendin:
#if defined(PENDIN)
      *b = ((t->c_lflag & PENDIN) == PENDIN);
      return 1;
#else
      return 0;
#endif
    case liexten:
#if defined(IEXTEN)
      *b = ((t->c_lflag & IEXTEN) == IEXTEN);
      return 1;
#else
      return 0;
#endif
    }
  return 0;
}

/* SetFlag - sets a flag value in, t, to, b, and returns TRUE if this
   flag value is supported.  */

int
EXPORT (SetFlag) (struct termios *t, Flag f, int b)
{
  switch (f)
    {
    case ignbrk:
#if defined(IGNBRK)
      return doSetUnset (&t->c_iflag, IGNBRK, b);
#else
      return 0;
#endif
    case ibrkint:
#if defined(BRKINT)
      return doSetUnset (&t->c_iflag, BRKINT, b);
#else
      return 0;
#endif
    case ignpar:
#if defined(IGNPAR)
      return doSetUnset (&t->c_iflag, IGNPAR, b);
#else
      return 0;
#endif
    case iparmrk:
#if defined(PARMRK)
      return doSetUnset (&t->c_iflag, PARMRK, b);
#else
      return 0;
#endif
    case inpck:
#if defined(INPCK)
      return doSetUnset (&t->c_iflag, INPCK, b);
#else
      return 0;
#endif
    case istrip:
#if defined(ISTRIP)
      return doSetUnset (&t->c_iflag, ISTRIP, b);
#else
      return 0;
#endif
    case inlcr:
#if defined(INLCR)
      return doSetUnset (&t->c_iflag, INLCR, b);
#else
      return 0;
#endif
    case igncr:
#if defined(IGNCR)
      return doSetUnset (&t->c_iflag, IGNCR, b);
#else
      return 0;
#endif
    case icrnl:
#if defined(ICRNL)
      return doSetUnset (&t->c_iflag, ICRNL, b);
#else
      return 0;
#endif
    case iuclc:
#if defined(IUCLC)
      return doSetUnset (&t->c_iflag, IUCLC, b);
#else
      return 0;
#endif
    case ixon:
#if defined(IXON)
      return doSetUnset (&t->c_iflag, IXON, b);
#else
      return 0;
#endif
    case ixany:
#if defined(IXANY)
      return doSetUnset (&t->c_iflag, IXANY, b);
#else
      return 0;
#endif
    case ixoff:
#if defined(IXOFF)
      return doSetUnset (&t->c_iflag, IXOFF, b);
#else
      return 0;
#endif
    case imaxbel:
#if defined(IMAXBEL)
      return doSetUnset (&t->c_iflag, IMAXBEL, b);
#else
      return 0;
#endif
    case opost:
#if defined(OPOST)
      return doSetUnset (&t->c_oflag, OPOST, b);
#else
      return 0;
#endif
    case olcuc:
#if defined(OLCUC)
      return doSetUnset (&t->c_oflag, OLCUC, b);
#else
      return 0;
#endif
    case onlcr:
#if defined(ONLCR)
      return doSetUnset (&t->c_oflag, ONLCR, b);
#else
      return 0;
#endif
    case ocrnl:
#if defined(OCRNL)
      return doSetUnset (&t->c_oflag, OCRNL, b);
#else
      return 0;
#endif
    case onocr:
#if defined(ONOCR)
      return doSetUnset (&t->c_oflag, ONOCR, b);
#else
      return 0;
#endif
    case onlret:
#if defined(ONLRET)
      return doSetUnset (&t->c_oflag, ONLRET, b);
#else
      return 0;
#endif
    case ofill:
#if defined(OFILL)
      return doSetUnset (&t->c_oflag, OFILL, b);
#else
      return 0;
#endif
    case ofdel:
#if defined(OFDEL)
      return doSetUnset (&t->c_oflag, OFDEL, b);
#else
      return 0;
#endif
    case onl0:
#if defined(NL0)
      return doSetUnset (&t->c_oflag, NL0, b);
#else
      return 0;
#endif
    case onl1:
#if defined(NL1)
      return doSetUnset (&t->c_oflag, NL1, b);
#else
      return 0;
#endif
    case ocr0:
#if defined(CR0)
      return doSetUnset (&t->c_oflag, CR0, b);
#else
      return 0;
#endif
    case ocr1:
#if defined(CR1)
      return doSetUnset (&t->c_oflag, CR1, b);
#else
      return 0;
#endif
    case ocr2:
#if defined(CR2)
      return doSetUnset (&t->c_oflag, CR2, b);
#else
      return 0;
#endif
    case ocr3:
#if defined(CR3)
      return doSetUnset (&t->c_oflag, CR3, b);
#else
      return 0;
#endif
    case otab0:
#if defined(TAB0)
      return doSetUnset (&t->c_oflag, TAB0, b);
#else
      return 0;
#endif
    case otab1:
#if defined(TAB1)
      return doSetUnset (&t->c_oflag, TAB1, b);
#else
      return 0;
#endif
    case otab2:
#if defined(TAB2)
      return doSetUnset (&t->c_oflag, TAB2, b);
#else
      return 0;
#endif
    case otab3:
#if defined(TAB3)
      return doSetUnset (&t->c_oflag, TAB3, b);
#else
      return 0;
#endif
    case obs0:
#if defined(BS0)
      return doSetUnset (&t->c_oflag, BS0, b);
#else
      return 0;
#endif
    case obs1:
#if defined(BS1)
      return doSetUnset (&t->c_oflag, BS1, b);
#else
      return 0;
#endif
    case off0:
#if defined(FF0)
      return doSetUnset (&t->c_oflag, FF0, b);
#else
      return 0;
#endif
    case off1:
#if defined(FF1)
      return doSetUnset (&t->c_oflag, FF1, b);
#else
      return 0;
#endif
    case ovt0:
#if defined(VT0)
      return doSetUnset (&t->c_oflag, VT0, b);
#else
      return 0;
#endif
    case ovt1:
#if defined(VT1)
      return doSetUnset (&t->c_oflag, VT1, b);
#else
      return 0;
#endif
    case b0:
#if defined(B0)
      return doSetUnset (&t->c_cflag, B0, b);
#else
      return 0;
#endif
    case b50:
#if defined(B50)
      return doSetUnset (&t->c_cflag, B50, b);
#else
      return 0;
#endif
    case b75:
#if defined(B75)
      return doSetUnset (&t->c_cflag, B75, b);
#else
      return 0;
#endif
    case b110:
#if defined(B110)
      return doSetUnset (&t->c_cflag, B110, b);
#else
      return 0;
#endif
    case b135:
#if defined(B134)
      return doSetUnset (&t->c_cflag, B134, b);
#else
      return 0;
#endif
    case b150:
#if defined(B150)
      return doSetUnset (&t->c_cflag, B150, b);
#else
      return 0;
#endif
    case b200:
#if defined(B200)
      return doSetUnset (&t->c_cflag, B200, b);
#else
      return 0;
#endif
    case b300:
#if defined(B300)
      return doSetUnset (&t->c_cflag, B300, b);
#else
      return 0;
#endif
    case b600:
#if defined(B600)
      return doSetUnset (&t->c_cflag, B600, b);
#else
      return 0;
#endif
    case b1200:
#if defined(B1200)
      return doSetUnset (&t->c_cflag, B1200, b);
#else
      return 0;
#endif
    case b1800:
#if defined(B1800)
      return doSetUnset (&t->c_cflag, B1800, b);
#else
      return 0;
#endif
    case b2400:
#if defined(B2400)
      return doSetUnset (&t->c_cflag, B2400, b);
#else
      return 0;
#endif
    case b4800:
#if defined(B4800)
      return doSetUnset (&t->c_cflag, B4800, b);
#else
      return 0;
#endif
    case b9600:
#if defined(B9600)
      return doSetUnset (&t->c_cflag, B9600, b);
#else
      return 0;
#endif
    case b19200:
#if defined(B19200)
      return doSetUnset (&t->c_cflag, B19200, b);
#else
      return 0;
#endif
    case b38400:
#if defined(B38400)
      return doSetUnset (&t->c_cflag, B38400, b);
#else
      return 0;
#endif
    case b57600:
#if defined(B57600)
      return doSetUnset (&t->c_cflag, B57600, b);
#else
      return 0;
#endif
    case b115200:
#if defined(B115200)
      return doSetUnset (&t->c_cflag, B115200, b);
#else
      return 0;
#endif
    case b240400:
#if defined(B230400)
      return doSetUnset (&t->c_cflag, B230400, b);
#else
      return 0;
#endif
    case b460800:
#if defined(B460800)
      return doSetUnset (&t->c_cflag, B460800, b);
#else
      return 0;
#endif
    case b500000:
#if defined(B500000)
      return doSetUnset (&t->c_cflag, B500000, b);
#else
      return 0;
#endif
    case b576000:
#if defined(B576000)
      return doSetUnset (&t->c_cflag, B576000, b);
#else
      return 0;
#endif
    case b921600:
#if defined(B921600)
      return doSetUnset (&t->c_cflag, B921600, b);
#else
      return 0;
#endif
    case b1000000:
#if defined(B1000000)
      return doSetUnset (&t->c_cflag, B1000000, b);
#else
      return 0;
#endif
    case b1152000:
#if defined(B1152000)
      return doSetUnset (&t->c_cflag, B1152000, b);
#else
      return 0;
#endif
    case b1500000:
#if defined(B1500000)
      return doSetUnset (&t->c_cflag, B1500000, b);
#else
      return 0;
#endif
    case b2000000:
#if defined(B2000000)
      return doSetUnset (&t->c_cflag, B2000000, b);
#else
      return 0;
#endif
    case b2500000:
#if defined(B2500000)
      return doSetUnset (&t->c_cflag, B2500000, b);
#else
      return 0;
#endif
    case b3000000:
#if defined(B3000000)
      return doSetUnset (&t->c_cflag, B3000000, b);
#else
      return 0;
#endif
    case b3500000:
#if defined(B3500000)
      return doSetUnset (&t->c_cflag, B3500000, b);
#else
      return 0;
#endif
    case b4000000:
#if defined(B4000000)
      return doSetUnset (&t->c_cflag, B4000000, b);
#else
      return 0;
#endif
    case maxbaud:
#if defined(__MAX_BAUD)
      return doSetUnset (&t->c_cflag, __MAX_BAUD, b);
#else
      return 0;
#endif
    case crtscts:
#if defined(CRTSCTS)
      return doSetUnset (&t->c_cflag, CRTSCTS, b);
#else
      return 0;
#endif
    case cs5:
#if defined(CS5)
      return doSetUnset (&t->c_cflag, CS5, b);
#else
      return 0;
#endif
    case cs6:
#if defined(CS6)
      return doSetUnset (&t->c_cflag, CS6, b);
#else
      return 0;
#endif
    case cs7:
#if defined(CS7)
      return doSetUnset (&t->c_cflag, CS7, b);
#else
      return 0;
#endif
    case cs8:
#if defined(CS8)
      return doSetUnset (&t->c_cflag, CS8, b);
#else
      return 0;
#endif
    case cstopb:
#if defined(CSTOPB)
      return doSetUnset (&t->c_cflag, CSTOPB, b);
#else
      return 0;
#endif
    case cread:
#if defined(CREAD)
      return doSetUnset (&t->c_cflag, CREAD, b);
#else
      return 0;
#endif
    case parenb:
#if defined(PARENB)
      return doSetUnset (&t->c_cflag, PARENB, b);
#else
      return 0;
#endif
    case parodd:
#if defined(PARODD)
      return doSetUnset (&t->c_cflag, PARODD, b);
#else
      return 0;
#endif
    case hupcl:
#if defined(HUPCL)
      return doSetUnset (&t->c_cflag, HUPCL, b);
#else
      return 0;
#endif
    case clocal:
#if defined(CLOCAL)
      return doSetUnset (&t->c_cflag, CLOCAL, b);
#else
      return 0;
#endif
    case lisig:
#if defined(ISIG)
      return doSetUnset (&t->c_lflag, ISIG, b);
#else
      return 0;
#endif
    case licanon:
#if defined(ICANON)
      return doSetUnset (&t->c_lflag, ICANON, b);
#else
      return 0;
#endif
    case lxcase:
#if defined(XCASE)
      return doSetUnset (&t->c_lflag, XCASE, b);
#else
      return 0;
#endif
    case lecho:
#if defined(ECHO)
      return doSetUnset (&t->c_lflag, ECHO, b);
#else
      return 0;
#endif
    case lechoe:
#if defined(ECHOE)
      return doSetUnset (&t->c_lflag, ECHOE, b);
#else
      return 0;
#endif
    case lechok:
#if defined(ECHOK)
      return doSetUnset (&t->c_lflag, ECHOK, b);
#else
      return 0;
#endif
    case lechonl:
#if defined(ECHONL)
      return doSetUnset (&t->c_lflag, ECHONL, b);
#else
      return 0;
#endif
    case lnoflsh:
#if defined(NOFLSH)
      return doSetUnset (&t->c_lflag, NOFLSH, b);
#else
      return 0;
#endif
    case ltopstop:
#if defined(TOSTOP)
      return doSetUnset (&t->c_lflag, TOSTOP, b);
#else
      return 0;
#endif
    case lechoctl:
#if defined(ECHOCTL)
      return doSetUnset (&t->c_lflag, ECHOCTL, b);
#else
      return 0;
#endif
    case lechoprt:
#if defined(ECHOPRT)
      return doSetUnset (&t->c_lflag, ECHOPRT, b);
#else
      return 0;
#endif
    case lechoke:
#if defined(ECHOKE)
      return doSetUnset (&t->c_lflag, ECHOKE, b);
#else
      return 0;
#endif
    case lflusho:
#if defined(FLUSHO)
      return doSetUnset (&t->c_lflag, FLUSHO, b);
#else
      return 0;
#endif
    case lpendin:
#if defined(PENDIN)
      return doSetUnset (&t->c_lflag, PENDIN, b);
#else
      return 0;
#endif
    case liexten:
#if defined(IEXTEN)
      return doSetUnset (&t->c_lflag, IEXTEN, b);
#else
      return 0;
#endif
    }
  return 0;
}

/* GetChar - sets a CHAR, ch, value from, t, and returns TRUE if this
   value is supported.  */

int
EXPORT (GetChar) (struct termios *t, ControlChar c, char *ch)
{
  switch (c)
    {

    case vintr:
#if defined(VINTR)
      *ch = t->c_cc[VINTR];
      return 1;
#else
      return 0;
#endif
    case vquit:
#if defined(VQUIT)
      *ch = t->c_cc[VQUIT];
      return 1;
#else
      return 0;
#endif
    case verase:
#if defined(VERASE)
      *ch = t->c_cc[VERASE];
      return 1;
#else
      return 0;
#endif
    case vkill:
#if defined(VKILL)
      *ch = t->c_cc[VKILL];
      return 1;
#else
      return 0;
#endif
    case veof:
#if defined(VEOF)
      *ch = t->c_cc[VEOF];
      return 1;
#else
      return 0;
#endif
    case vtime:
#if defined(VTIME)
      *ch = t->c_cc[VTIME];
      return 1;
#else
      return 0;
#endif
    case vmin:
#if defined(VMIN)
      *ch = t->c_cc[VMIN];
      return 1;
#else
      return 0;
#endif
    case vswtc:
#if defined(VSWTC)
      *ch = t->c_cc[VSWTC];
      return 1;
#else
      return 0;
#endif
    case vstart:
#if defined(VSTART)
      *ch = t->c_cc[VSTART];
      return 1;
#else
      return 0;
#endif
    case vstop:
#if defined(VSTOP)
      *ch = t->c_cc[VSTOP];
      return 1;
#else
      return 0;
#endif
    case vsusp:
#if defined(VSUSP)
      *ch = t->c_cc[VSUSP];
      return 1;
#else
      return 0;
#endif
    case veol:
#if defined(VEOL)
      *ch = t->c_cc[VEOL];
      return 1;
#else
      return 0;
#endif
    case vreprint:
#if defined(VREPRINT)
      *ch = t->c_cc[VREPRINT];
      return 1;
#else
      return 0;
#endif
    case vdiscard:
#if defined(VDISCARD)
      *ch = t->c_cc[VDISCARD];
      return 1;
#else
      return 0;
#endif
    case vwerase:
#if defined(VWERASE)
      *ch = t->c_cc[VWERASE];
      return 1;
#else
      return 0;
#endif
    case vlnext:
#if defined(VLNEXT)
      *ch = t->c_cc[VLNEXT];
      return 1;
#else
      return 0;
#endif
    case veol2:
#if defined(VEOL2)
      *ch = t->c_cc[VEOL2];
      return 1;
#else
      return 0;
#endif
    default:
      return 0;
    }
}

/* SetChar - sets a CHAR value in, t, and returns TRUE if, c, is
   supported.  */

int
EXPORT (SetChar) (struct termios *t, ControlChar c, char ch)
{
  switch (c)
    {

    case vintr:
#if defined(VINTR)
      t->c_cc[VINTR] = ch;
      return 1;
#else
      return 0;
#endif
    case vquit:
#if defined(VQUIT)
      t->c_cc[VQUIT] = ch;
      return 1;
#else
      return 0;
#endif
    case verase:
#if defined(VERASE)
      t->c_cc[VERASE] = ch;
      return 1;
#else
      return 0;
#endif
    case vkill:
#if defined(VKILL)
      t->c_cc[VKILL] = ch;
      return 1;
#else
      return 0;
#endif
    case veof:
#if defined(VEOF)
      t->c_cc[VEOF] = ch;
      return 1;
#else
      return 0;
#endif
    case vtime:
#if defined(VTIME)
      t->c_cc[VTIME] = ch;
      return 1;
#else
      return 0;
#endif
    case vmin:
#if defined(VMIN)
      t->c_cc[VMIN] = ch;
      return 1;
#else
      return 0;
#endif
    case vswtc:
#if defined(VSWTC)
      t->c_cc[VSWTC] = ch;
      return 1;
#else
      return 0;
#endif
    case vstart:
#if defined(VSTART)
      t->c_cc[VSTART] = ch;
      return 1;
#else
      return 0;
#endif
    case vstop:
#if defined(VSTOP)
      t->c_cc[VSTOP] = ch;
      return 1;
#else
      return 0;
#endif
    case vsusp:
#if defined(VSUSP)
      t->c_cc[VSUSP] = ch;
      return 1;
#else
      return 0;
#endif
    case veol:
#if defined(VEOL)
      t->c_cc[VEOL] = ch;
      return 1;
#else
      return 0;
#endif
    case vreprint:
#if defined(VREPRINT)
      t->c_cc[VREPRINT] = ch;
      return 1;
#else
      return 0;
#endif
    case vdiscard:
#if defined(VDISCARD)
      t->c_cc[VDISCARD] = ch;
      return 1;
#else
      return 0;
#endif
    case vwerase:
#if defined(VWERASE)
      t->c_cc[VWERASE] = ch;
      return 1;
#else
      return 0;
#endif
    case vlnext:
#if defined(VLNEXT)
      t->c_cc[VLNEXT] = ch;
      return 1;
#else
      return 0;
#endif
    case veol2:
#if defined(VEOL2)
      t->c_cc[VEOL2] = ch;
      return 1;
#else
      return 0;
#endif
    default:
      return 0;
    }
}

void
_M2_termios_init (int argc, char *argv[], char *envp[])
{
}

void
_M2_termios_fini (int argc, char *argv[], char *envp[])
{
}

#ifdef __cplusplus
}
#endif
