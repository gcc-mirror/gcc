/* { dg-do compile } */

struct barstruct { char const* some_string; };

void changethepointer(struct barstruct***);

void baz()
{
  struct barstruct bar1;
  struct barstruct* barptr = &bar1;
  struct barstruct** barptr2 = &barptr;
  changethepointer(&barptr2);
  barptr->some_string = "Everything OK";
}

/* { dg-final { scan-assembler "Everything OK" } } */
