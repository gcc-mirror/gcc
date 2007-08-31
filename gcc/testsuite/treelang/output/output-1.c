/* Driver for treelang test pgm */

/*

  Copyright (C) 2001, 2002, 2007 Free Software Foundation, Inc.

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with GCC; see the file COPYING3.  If not see
  <http://www.gnu.org/licenses/>.
  
  In other words, you are welcome to use, share and improve this program.
  You are forbidden to forbid anyone else to use, share and improve
  what you give them.   Help stamp out software-hoarding!  
*/  

int add(int, int);
int subtract(int, int);
int first_nonzero(int, int);
extern int printf(char *template, ...);

int 
main (int argc, char *argv[])
{
  printf("2:%d\n", add(1,1));
  printf("7:%d\n", add(3,4));
  printf("-1:%d\n", subtract(3,4));
  printf("1:%d\n", subtract(2,1));
  printf("3:%d\n", first_nonzero(0,3));
  printf("0:%d\n", first_nonzero(0,0));
  printf("1:%d\n", first_nonzero(1,0));
  printf("15:%d\n", double_plus_one(7));
  return 0;
}
