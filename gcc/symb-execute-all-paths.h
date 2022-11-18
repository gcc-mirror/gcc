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
  /* A vector of states to keep the state of each executed path.  */
  vec<state*> states;

/* Assign symbolic values to the arguments of the function
   and keep in the state.  */
  static void make_symbolic_func_args_and_sizes (function *, state *);

  /* Add declared ssa variables to the state.  */
  static void add_function_local_ssa_vars (function *fun, state *initial_state);

  void execute_assign_statement (const gassign *);

  /* Execute gimple statements of the basic block.
   Keeping values of variables in the state.  */
  void execute_bb_gimple_statements (basic_block);

  /* Assign values of phi instruction to its result.
   Keep updated values in the state.  */
  void execute_bb_phi_statements (basic_block);

  /* Execute all statements of the basic block.
    Keeping values of variables in the state.  */
  void execute_bb_statements (basic_block);

/* Traverse function fun's all paths from the first basic block to the last.
   Each time iterate loops only once.
   Symbolically execute statements of each path.  */
  void traverse_function (function *);

 public:
  void execute_function (function *);

  crc_symb_execution ()
  {
    /* Reserve memory for the vector.
       Actually, if the function is calculating one CRC, there may be 2 states.
       Just in case allocate more memory.  */
    states.create (4);
  }
};

#endif //GCC_EXECUTE_ALL_PATHS_H
