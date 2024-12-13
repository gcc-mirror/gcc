/* Execute symbolically all paths of the loop.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Mariam Arutunian <mariamarutunian@gmail.com>

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
#include "sym-exec/sym-exec-state.h"

class crc_symbolic_execution {

 private:
  /* A vector of states to keep the current state of each executed path.  */
  vec<state *> m_states;

  /* A vector of final states
     to keep the returned_value and path conditions.  */
  vec<state *> m_final_states;

  /* Potential CRC loop, which must be executed symbolically,
     to check whether it calculates CRC.  */
  class loop *m_crc_loop;

  /* Output CRC from the last block of the loop.  */
  gphi *m_output_crc;

  /* Indicates whether the loop execution brought to loop exit.
     I.e. the condition of the loop is false.  */
  bool m_is_last_iteration;

  /* Returns true if the variable is used outside the loop.
     Otherwise, returns false.  */
  bool is_used_outside_the_loop (tree);

  /* Add next basic blocks of the conditional block
     for the execution path into the stack.
     If the condition depends on symbolic values, keep both edges.
     If the condition is true, keep true edge, else - false edge.
     Returns true if addition succeed.  Otherwise - false.  */
  bool add_next_bbs (basic_block, state *, auto_vec<edge> &);

  /* Keep conditions depending on symbolic variables in the states.  */
  static bool add_condition (const gcond *, state *, state *);

  /* The function adds E edge into the STACK if it doesn't have an immediate
     successor back edge.

     When loop counter is checked in the if condition,
     we mustn't continue on real path as we want to stop the execution before
     the second iteration.  */
  bool add_edge (edge, auto_vec<edge> &);

  /* Create new state for true and false branch.
     Keep conditions in new created states.  */
  bool resolve_condition (const gcond *, auto_vec<edge> &);

  /* If final states are less than two, adds new FINAL_STATE and returns true.
   Otherwise, returns false.
   In CRC cases we detect may not occur more than two final states.  */
  bool add_final_state (state *);

  /* Keep the state of the executed path in final states.  */
  bool keep_states ();

  bool execute_assign_statement (const gassign *);

  /* Execute gimple statements of the basic block.
     Keeping values of variables in the state.  */
  bool execute_bb_gimple_statements (basic_block, auto_vec<edge> &);

  /* Assign values of phi instruction to its result.
     Keep updated values in the state.  */
  bool execute_bb_phi_statements (basic_block, edge);

  /* Execute all statements of the basic block.
    Keeping values of variables in the state.  */
  bool execute_bb_statements (basic_block, edge, auto_vec<edge> &);

  /* Create initial state of the loop's header BB variables which have constant
   values.
   If it is the first iteration of the loop, initialise variables with the
   initial values, otherwise initialise the variable with the value calculated
   during the previous execution.  */
  state *create_initial_state (class loop *);

/* Traverse function fun's all paths from the first basic block to the last.
   Each time iterate loops only once.
   Symbolically execute statements of each path.  */
  bool traverse_function (function *);

  /* Execute the loop, which calculates crc with initial values,
   to calculate the polynomial.  */
  bool execute_crc_loop (gphi *, gphi *, bool);

 public:

  /* Returns calculated polynomial by executing the loop
     with concrete values.
     First value of the pair is the tree containing the value of the polynomial,
     second is the calculated polynomial.  The pair may contain nullptr.  */
  std::pair <tree, value *>
  extract_polynomial (gphi *, gphi *, tree, bool);

  /* Symbolically execute the CRC loop, doing one iteration.  */
  bool symb_execute_crc_loop ();

  const vec<state *> &get_final_states ()
  {
    return m_final_states;
  }

  bool is_last_iteration ()
  {
    return m_is_last_iteration;
  }

  crc_symbolic_execution (class loop *loop, gphi * output_crc) :
      m_crc_loop (loop), m_output_crc (output_crc), m_is_last_iteration (false)
  {
    /* Reserve memory for the vectors of states.  */
    int max_states = 2;
    m_states.create (max_states);
    m_final_states.create (max_states);
  }

  ~crc_symbolic_execution ()
  {
    /* Free memory.  */
    state::clear_states (&m_states);
    state::clear_states (&m_final_states);
  }
};


/**************************** LFSR MATCHING *********************************/

/* Returns true if all states match the LFSR, otherwise - false.  */
bool all_states_match_lfsr (value *, bool, tree, const vec<state *> &);


#endif //GCC_CRC_VERIFICATION
