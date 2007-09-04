/* { dg-do compile } */
/* { dg-options -O2 } */

extern void do_something_usefull();
/* Check that we finish compiling even if instructed to
   flatten a cyclic callgraph.  Verify we correctly
   flatten with another function marked flatten in the
   callgraph.  */

void __attribute__((flatten)) direct(void)
{
  direct();
}


void __attribute__((flatten)) indirect(void);
static void indirect1(void)
{
  indirect();
}
void __attribute__((flatten)) indirect(void)
{
  indirect1();
}


void __attribute__((flatten)) doubleindirect(void);
static void doubleindirect2(void)
{
  doubleindirect();
  do_something_usefull ();
}
static void doubleindirect1(void)
{
  doubleindirect2();
}
void __attribute__((flatten)) doubleindirect(void)
{
  doubleindirect1();
}


static void subcycle1(void);
static void subcycle2(void)
{
  subcycle1();
  do_something_usefull ();
}
static void subcycle1(void)
{
  subcycle2();
}
void __attribute__((flatten)) subcycle(void)
{
  subcycle1();
}


static void doublesubcycle1(void);
static void doublesubcycle2(void);
static void doublesubcycle3(void)
{
  doublesubcycle1();
  do_something_usefull ();
}
static void doublesubcycle2(void)
{
  doublesubcycle3();
}
static void doublesubcycle1(void)
{
  doublesubcycle2();
}
void __attribute__((flatten)) doublesubcycle(void)
{
  doublesubcycle1();
}

/* { dg-final { scan-assembler "cycle\[123\]\[: \t\n\]" } } */
/* { dg-final { scan-assembler-not "indirect\[12\]\[: \t\n\]" } } */
