/* Callgraph handling code.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_CGRAPH_H
#define GCC_CGRAPH_H

/* Information about the function collected locally.
   Available after function is lowered  */

struct cgraph_local_info
{
  /* Set when function function is visiable in current compilation unit only
     and it's address is never taken.  */
  bool local;
  bool inline_many;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-time.  */

struct cgraph_global_info
{
  /* Empty for the moment.  */
  int dummy;
};


/* The cgraph data strutcture.
   Each function decl has assigned cgraph_node listing calees and callers.  */

struct cgraph_node
{
  tree decl;
  struct cgraph_edge *callees;
  struct cgraph_edge *callers;
  struct cgraph_node *next;
  /* For nested functions points to function the node is nested in.  */
  struct cgraph_node *origin;
  /* Points to first nested function, if any.  */
  struct cgraph_node *nested;
  /* Pointer to the next function with same origin, if any.  */
  struct cgraph_node *next_nested;
  void *aux;

  /* Set when function must be output - it is externally visible
     or it's address is taken.  */
  bool needed;
  /* Set when function is reachable by call from other function
     that is eighter reachable or needed.  */
  bool reachable;
  /* Set when the frontend has been asked to lower representation of this
     function into trees.  Callees lists are not available when lowered
     is not set.  */
  bool lowered;
  /* Set when function is scheduled to be assembled.  */
  bool output;
  struct cgraph_local_info local;
  struct cgraph_global_info global;
};

struct cgraph_edge
{
  struct cgraph_node *caller, *callee;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *next_callee;
};

extern struct cgraph_node *cgraph_nodes;
extern int cgraph_n_nodes;
extern bool cgraph_global_info_ready;

/* In cgraph.c  */
void dump_cgraph			PARAMS ((FILE *));
void cgraph_remove_call			PARAMS ((tree, tree));
struct cgraph_edge *cgraph_record_call	PARAMS ((tree, tree));
struct cgraph_node *cgraph_node		PARAMS ((tree decl));
bool cgraph_calls_p			PARAMS ((tree, tree));
struct cgraph_local_info *cgraph_local_info PARAMS ((tree));
struct cgraph_global_info *cgraph_global_info PARAMS ((tree));

/* In cgraphunit.c  */
void cgraph_finalize_function		PARAMS ((tree, tree));
void cgraph_finalize_compilation_unit	PARAMS ((void));
void cgraph_create_edges		PARAMS ((tree, tree));
void cgraph_optimize			PARAMS ((void));
void cgraph_mark_needed_node		PARAMS ((struct cgraph_node *, int));

#endif  /* GCC_CGRAPH_H  */
