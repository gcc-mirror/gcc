/* Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Invalid __ea declarations.  */

/* { dg-do compile } */

__ea char ea_str[] = "abc";
char lm_str[] = "abc";

__ea char *lm_ea_ptr1 = "abc";				/* { dg-error "initializer element is not computable at load time" } */
__ea char *lm_ea_ptr2 = (__ea char *)"abc";		/* { dg-error "initializer element is not constant" } */
__ea char *lm_ea_ptr3 = ea_str;
__ea char *lm_ea_ptr4 = (__ea char *)ea_str;
__ea char *lm_ea_ptr5 = lm_str;				/* { dg-error "initializer element is not computable at load time" } */
__ea char *lm_ea_ptr6 = (__ea char *)lm_str;		/* { dg-error "initializer element is not constant" } */

__ea char * __ea ea_ea_ptr1 = ea_str;
__ea char * __ea ea_ea_ptr2 = (__ea char *)ea_str;

char * __ea ea_lm_ptr1 = lm_str;
char * __ea ea_lm_ptr2 = (char *)lm_str;

struct foo {
  int first;
  __ea char *ptr;
  int last;
};

__ea struct foo ea_struct1 = {
  10,
  (__ea char *)0,
  11,
};

__ea struct foo ea_struct2 = {
  20,
  0,
  21,
};

struct foo ea_struct3 = {
  30,
  ea_str,
  31,
};

struct foo ea_struct4 = {
  40,
  (__ea char *)lm_str,	/* { dg-error "(initializer element is not constant)|(near initialization)" "" } */
  41,
};

struct bar {
  int first;
  char *ptr;
  int last;
};

__ea struct bar ea_struct5 = {
  50,
  0,
  51,
};

__ea struct bar ea_struct6 = {
  60,
  (char *)0,
  61,
};

__ea struct bar ea_struct7 = {
  70,
  lm_str,
  71,
};

struct bar lm_struct8 = {
  80,
  0,
  81,
};

struct bar lm_struct9 = {
  90,
  (char *)0,
  91,
};

struct bar lm_struct10 = {
  100,
  lm_str,
  101,
};
