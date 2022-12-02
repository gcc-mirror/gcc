/*
   Execute symbolically all paths of the function.  Iterate loops only once.
   Copyright (C) 2006-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.   */

#ifndef GCC_EXECUTE_ALL_PATHS_H
#define GCC_EXECUTE_ALL_PATHS_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "cfgloop.h"
#include "gimple-range.h"
#include "tree-scalar-evolution.h"
#include "hwint.h"
#include "function.h"
#include "sym-exec/state.h"

class crc_symb_execution {

 private:
  /* A vector of states to keep the current state of each executed path.  */
  vec<state*> states;

  /* A vector of final states
     to keep the returned_value and path conditions.  */
  vec<state*> final_states;

/* Assign symbolic values to the arguments of the function
   and keep in the state.  */
  static bool make_symbolic_func_args_and_sizes (function *, state *);

  /* Add declared ssa variables to the state.  */
  static bool add_function_local_ssa_vars (function *, state *);

  bool execute_assign_statement (const gassign *);

  /* Add next basic blocks of the conditional block
     for the execution path into the stack.  */
  void add_next_bbs (basic_block, state *, auto_vec<edge, 20>&);

  /* Keep conditions depending on symbolic variables in the states.  */
  static bool add_condition (const gcond*, state*, state*);

  /* Create new state for true and false branch.
     Keep conditions in new created states.  */
  bool resolve_condition (const gcond*, auto_vec<edge, 20>&);

  /* Keep the calculated value of the return value
     and the conditions of the executed path.  */
  bool keep_return_val_and_conditions (const greturn*);

  /* Execute gimple statements of the basic block.
     Keeping values of variables in the state.  */
  bool execute_bb_gimple_statements (basic_block, auto_vec<edge, 20>&);

  /* Assign values of phi instruction to its result.
     Keep updated values in the state.  */
  bool execute_bb_phi_statements (basic_block, edge);

  /* Execute all statements of the basic block.
    Keeping values of variables in the state.  */
  bool execute_bb_statements (basic_block, edge, auto_vec<edge, 20>&);

/* Traverse function fun's all paths from the first basic block to the last.
   Each time iterate loops only once.
   Symbolically execute statements of each path.  */
  bool traverse_function (function *);

 public:
  bool execute_function (function *);

  crc_symb_execution ()
  {
    /* Reserve memory for the vectors of states.  */
    states.create (30);
    final_states.create (30);
  }

  ~crc_symb_execution ()
  {
     /* Free memory.  */
    states.release ();
    final_states.release ();
  }
};

#endif //GCC_EXECUTE_ALL_PATHS_H
