/* Definitions for Bytecode Interpreter.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


struct node
{
  char *text;
  struct node *next;
};

struct variation
{
  char *name;
  int code;
  struct node *inputs;
  struct node *outputs;
  struct node *literals;
  struct variation *next;
};

struct def
{
  char *basename;
  char *template;
  struct variation *variations;
  struct def *next;
};

extern struct def *defs;
extern int ndefs;
extern void reverse();
