/* { dg-do compile } */
/* { dg-options "-O2 -mtune=xscale" } */

typedef unsigned int speed_t;
typedef unsigned int tcflag_t;

struct termios {
 tcflag_t c_cflag;
};

speed_t
cfgetospeed (const struct termios *tp)
{
  return tp->c_cflag & 010017;
}
