/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */
/* Test read and write on all basic types.  */

static char gc;
static signed char gsc;
static unsigned char guc;

static short gs;
static unsigned short gus;

static int gi;
static unsigned int gui;

static long gl;
static unsigned long gul;

static long long gll;
static unsigned long long gull;

static float gf;
static double gd;
static long double gld;

void f(void)
{
  __transaction_atomic {
    gc++;
    gsc++;
    guc++;

    gs++;
    gus++;

    gi++;
    gui++;

    gl++;
    gul++;

    gll++;
    gull++;

    gf++;
    gd++;
    gld++;
  }
}
