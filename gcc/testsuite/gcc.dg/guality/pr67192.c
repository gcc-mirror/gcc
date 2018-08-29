/* PR debug/67192 */
/* { dg-do run } */
/* { dg-options "-g -Wmisleading-indentation" } */

volatile int cnt = 0;

__attribute__((noinline, noclone)) static int
last (void)
{
  return ++cnt % 5 == 0;
}

__attribute__((noinline, noclone)) static void
do_it (void)
{
  asm volatile ("" : : "r" (&cnt) : "memory");
}

__attribute__((noinline, noclone)) static void
f1 (void)
{
  for (;; do_it())
    {
      if (last ())
	break;
    }
  do_it (); /* { dg-final { gdb-test . "cnt" "5" } } */
}

__attribute__((noinline, noclone)) static void
f2 (void)
{
  while (1)
    {
      if (last ())
	break;
      do_it ();
    }
  do_it (); /* { dg-final { gdb-test . "cnt" "10" } } */
}

__attribute__((noinline, noclone)) static void
f3 (void)
{
  for (;; do_it())
    if (last ())
      break;
  do_it (); /* { dg-final { gdb-test . "cnt" "15" } } */
}

__attribute__((noinline, noclone)) static void
f4 (void)
{
  while (1) /* { dg-final { gdb-test . "cnt" "15" } } */
    if (last ())
      break;
    else
      do_it ();
  do_it (); /* { dg-final { gdb-test . "cnt" "20" } } */
}

void (*volatile fnp1) (void) = f1;
void (*volatile fnp2) (void) = f2;
void (*volatile fnp3) (void) = f3;
void (*volatile fnp4) (void) = f4;

int
main ()
{
  asm volatile ("" : : "r" (&fnp1) : "memory");
  asm volatile ("" : : "r" (&fnp2) : "memory");
  asm volatile ("" : : "r" (&fnp3) : "memory");
  asm volatile ("" : : "r" (&fnp4) : "memory");
  fnp1 ();
  fnp2 ();
  fnp3 ();
  fnp4 ();
  return 0;
}
