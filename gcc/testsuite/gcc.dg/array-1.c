/* Simplified from ncurses 5.0's pad.c
   by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

   Copyright (C) 1999 Free Software Foundation  */

/* { dg-do compile } */

extern char foo[1];
char foo[] = "";

int
bar()
{
  return foo[0];
}
