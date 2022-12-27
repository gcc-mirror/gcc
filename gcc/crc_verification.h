/*
   Execute symbolically all paths of the function, iterating loops only once.
   Calculate the value of the polynomial by executing loop with real values.
   Check calculated final states of return values match determined LFSR.
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

#ifndef GCC_CRC_VERIFICATION
#define GCC_CRC_VERIFICATION

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfgloop.h"
#include "sym-exec/state.h"

class crc_symbolic_execution {

 private:
  /* A vector of states to keep the current state of each executed path.  */
  vec<state *> states;

  /* A vector of final states
     to keep the returned_value and path conditions.  */
  vec<state *> final_states;

/* Assign symbolic values to the arguments of the function
   and keep in the state.  */
  static bool make_symbolic_func_args_and_sizes (function *, state *);

  /* Add declared ssa variables to the state.  */
  static bool add_function_local_ssa_vars (function *, state *);

  bool execute_assign_statement (const gassign *);

  /* Add next basic blocks of the conditional block
     for the execution path into the stack.  */
  void add_next_bbs (basic_block, state *, auto_vec<edge, 20> &);

  /* Keep conditions depending on symbolic variables in the states.  */
  static bool add_condition (const gcond *, state *, state *);

  /* Create new state for true and false branch.
     Keep conditions in new created states.  */
  bool resolve_condition (const gcond *, auto_vec<edge, 20> &);

  /* Keep the calculated value of the return value
     and the conditions of the executed path.  */
  bool keep_return_val_and_conditions (const greturn *);

  /* Execute gimple statements of the basic block.
     Keeping values of variables in the state.  */
  bool execute_bb_gimple_statements (basic_block, auto_vec<edge, 20> &);

  /* Assign values of phi instruction to its result.
     Keep updated values in the state.  */
  bool execute_bb_phi_statements (basic_block, edge);

  /* Execute all statements of the basic block.
    Keeping values of variables in the state.  */
  bool execute_bb_statements (basic_block, edge, auto_vec<edge, 20> &);

/* Traverse function fun's all paths from the first basic block to the last.
   Each time iterate loops only once.
   Symbolically execute statements of each path.  */
  bool traverse_function (function *);

  /* Execute the loop, which calculates crc with initial values,
   to calculate the polynomial.  */
  bool execute_crc_loop (class loop *, gphi *, gphi *, bool);

 public:

  /* Symbolically execute the function and keep final states.  */
  bool execute (function *);

  /* Returns calculated polynomial by executing the loop
     with concrete values.  */
  value *extract_poly_and_create_lfsr (class loop *, gphi *, gphi *, bool);

  const vec<state *> &get_final_states ()
  {
    return final_states;
  }

  crc_symbolic_execution ()
  {
    /* Reserve memory for the vectors of states.  */
    states.create (30);
    final_states.create (30);
  }

  ~crc_symbolic_execution ()
  {
    /* Free memory.  */
    state::clear_states (&states);
    state::clear_states (&final_states);
  }
};


/**************************** LFSR MATCHING *********************************/
// TODO write in separate file

/* Returns true if all states match the LFSR, otherwise - false.  */
bool
all_states_match_lfsr (value *,
		       bool,
		       const vec<state *> &);


/* Returns true if in the CRC value's vector we have one of the form of
   following consecutive bits
   1.  0, then 1
   2.  1, then 0
   3.  0, then 0
   4.  1, then 1
   Otherwise returns false.
   */
bool
maybe_neighbour_vals_of_crc (bit *, value_bit *);


/* Returns true if in the CRC value's vector we have one of the form of
   following consecutive bits
   1. crc[], then crc[]
   2. crc[], then crc[]^data[]
   3. crc[], then crc[]^data[]^1
   4. crc[], then crc[]^1
   5. data[], then crc[]^data[]
   6. data[], then crc[]^data[]^1
   7. crc[], then crc[]^data1[]^data2[] TODO: Maybe needs a correction.
   ...
   Otherwise returns false.  */
bool
maybe_neighbour_vals_of_crc (symbolic_bit *, value_bit *);


/* Returns true if in the CRC value's vector we have one of the form of
   following consecutive bits
   1. crc[]^1 or crc[]^data[]^1, then crc[]
   2. crc[]^data[]^1, then crc[]^data[]
   3. crc[]^data[], then crc[] or data[]
   4. crc[]^data[], then crc[]^data[]
   5. crc[]^data[], then crc[]^data[]^1
   Otherwise returns false.  */
bool
maybe_neighbour_vals_of_crc (bit_xor_expression *,
			     value_bit *);


#endif //GCC_CRC_VERIFICATION
