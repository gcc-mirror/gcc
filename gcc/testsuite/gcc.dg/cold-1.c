/* { dg-do compile { target nonpic } } */
/* { dg-options "-O2 -Wsuggest-attribute=cold" } */

extern void do_something_interesting_and_never_return ();

int
foo1(int a)  
{ /* { dg-warning "cold" "detect cold candidate" { target *-*-* } "8" } */ 
  if (a)
    abort ();
  else
    abort ();
}

int
foo2(int a)  
{ 
  if (a)
    do_something_interesting_and_never_return ();
  abort ();
}
