/* This small function uses all the arithmetic operators that
   libgcc1.c can handle.  If you can link it, then
   you have provided replacements for all the libgcc1.c functions that
   your target machine needs.  */

#include <stddef.h>

int foo ();
double dfoo ();
void discard (int);
void ddiscard (double);

/* We don't want __main here because that can drag in atexit (among other
   things) which won't necessarily exist yet.  */

int
main_without__main ()
{
  int a = foo (), b = foo ();
  unsigned int au = foo (), bu = foo ();
  float af = dfoo (), bf = dfoo ();
  double ad = dfoo (), bd = dfoo ();

  discard (a * b);
  discard (a / b);
  discard (a % b);

  discard (au / bu);
  discard (au % bu);

  discard (a >> b);
  discard (a << b);

  discard (au >> bu);
  discard (au << bu);

  ddiscard (ad + bd);
  ddiscard (ad - bd);
  ddiscard (ad * bd);
  ddiscard (ad / bd);
  ddiscard (-ad);

  ddiscard (af + bf);
  ddiscard (af - bf);
  ddiscard (af * bf);
  ddiscard (af / bf);
  ddiscard (-af);

  discard ((int) ad);
  discard ((int) af);

  ddiscard ((double) a);
  ddiscard ((float) a);
  ddiscard ((float) ad);

  discard (ad == bd);
  discard (ad < bd);
  discard (ad > bd);
  discard (ad != bd);
  discard (ad <= bd);
  discard (ad >= bd);

  discard (af == bf);
  discard (af < bf);
  discard (af > bf);
  discard (af != bf);
  discard (af <= bf);
  discard (af >= bf);

  return 0;
}

void
discard (x)
     int x __attribute__((__unused__));
{}

void
ddiscard (x)
     double x __attribute__((__unused__));
{}

int
foo ()
{
  static int table[] = {20, 69, 4, 12};
  static int idx;

  return table[idx++];
}

double
dfoo ()
{
  static double table[] = {20.4, 69.96, 4.4, 202.202};
  static int idx;

  return table[idx++];
}

/* Provide functions that some versions of the linker use to default
   the start address if -e symbol is not used, to avoid the warning
   message saying the start address is defaulted.  */
extern void start() __asm__("start");
extern void _start() __asm__("_start");
extern void __start() __asm__("__start");

/* Provide functions that might be needed by soft-float emulation routines.  */
void *memcpy(void *to,
	     const void *from __attribute__((__unused__)),
	     size_t len __attribute__((__unused__)))
{
  return to;
}

void start() {}
void _start() {}
void __start() {}
void mainCRTStartup() {}
