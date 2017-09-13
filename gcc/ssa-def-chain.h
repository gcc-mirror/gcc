/* Header file for ssa definition chain.
   Copyright (C) 2017 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_SSA_DEF_CHAIN_H
#define GCC_SSA_DEF_CHAIN_H


/* This class is used to maintain a definition chain of SSA_NAMEs.
   An SSA_NAME appears in a definition chain if it is used in a calcuation
   that is is supported by the optimization using the data structure.
   The routine "generate_def_chain" determines this.

   Initially this is being utilized by the ssa range generator to determine  
   which expressions can be re-expressed.
   
   Assuming operators -,+ and * are understood, using the follwing snippet:

	 b_6 = t_2 << 6
	 a_3 = b_6 + 4
	 c_7 = a_3 * 2
	 q_9 = -c_7

   the definition chain for a_3 contains only b_6.  
   the definition chain for c_7 contains both a_3 and b_6.
   the definition chain for q_9 contains c_7, a_3, and b_6.

   The range operator class does not understand the << operator, so the
   defintion chain is terminated when the stmt defining b_6 is encountered
   using an unknow operator. 
   
   This means the "terminal" node in each of the chains shown is b_6. This is
   the first value used wihch cannot be calculated.

   If there is more than one possible terminal name, ie both b_6 and q_9 could
   be considered terminal in this case:
         q_9 = foo ()
	 b_6 = t_2 << 6
	 a_3 = b_6 + q_9 
   the last confirmed single name is used... in this case a_3.
   
   These are calculated on demand and cached.  */
   
class ssa_define_chain
{
  bool bb_exclusive;
  vec<bitmap> def_chain;
  vec<tree> terminal;
  tree process_op (tree operand, unsigned version, basic_block bb);
  tree generate_def_chain (tree name);
public:
  ssa_define_chain (bool within_bb = true);
  bitmap operator[](tree name);
  bitmap operator[](unsigned index);
  tree terminal_name (tree name);
  tree terminal_name (unsigned index);
  bool in_chain_p (tree def, tree name);
  void dump (FILE *f);
};

#endif /* GCC_SSA_DEF_CHAIN_H */
