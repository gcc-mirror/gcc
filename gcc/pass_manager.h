/* pass_manager.h - The pipeline of optimization passes
   Copyright (C) 2013 Free Software Foundation, Inc.

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

#ifndef GCC_PASS_MANAGER_H
#define GCC_PASS_MANAGER_H

class opt_pass;
struct register_pass_info;

/* Define a list of pass lists so that both passes.c and plugins can easily
   find all the pass lists.  */
#define GCC_PASS_LISTS \
  DEF_PASS_LIST (all_lowering_passes) \
  DEF_PASS_LIST (all_small_ipa_passes) \
  DEF_PASS_LIST (all_regular_ipa_passes) \
  DEF_PASS_LIST (all_lto_gen_passes) \
  DEF_PASS_LIST (all_passes)

#define DEF_PASS_LIST(LIST) PASS_LIST_NO_##LIST,
enum pass_list
{
  GCC_PASS_LISTS
  PASS_LIST_NUM
};
#undef DEF_PASS_LIST

namespace gcc {

class context;

class pass_manager
{
public:
  pass_manager(context *ctxt);

  void register_pass (struct register_pass_info *pass_info);
  void register_one_dump_file (struct opt_pass *pass);

  opt_pass *get_pass_for_id (int id) const;

  void dump_passes () const;

  void dump_profile_report () const;

  void finish_optimization_passes ();

  /* Access to specific passes, so that the majority can be private.  */
  void execute_early_local_passes ();
  unsigned int execute_pass_mode_switching ();

public:
  /* The root of the compilation pass tree, once constructed.  */
  opt_pass *all_passes;
  opt_pass *all_small_ipa_passes;
  opt_pass *all_lowering_passes;
  opt_pass *all_regular_ipa_passes;
  opt_pass *all_lto_gen_passes;
  opt_pass *all_late_ipa_passes;

  /* A map from static pass id to optimization pass.  */
  opt_pass **passes_by_id;
  int passes_by_id_size;

  opt_pass **pass_lists[PASS_LIST_NUM];

private:
  void set_pass_for_id (int id, opt_pass *pass);
  int register_dump_files_1 (struct opt_pass *pass, int properties);
  void register_dump_files (struct opt_pass *pass, int properties);

private:
  context *ctxt_;

  /* References to all of the individual passes.
     These fields are generated via macro expansion.

     For example:
         NEXT_PASS (pass_build_cfg, 1);
     within pass-instances.def means that there is a field:
         opt_pass *pass_build_cfg_1;

     Similarly, the various:
        NEXT_PASS (pass_copy_prop, 1);
        ...
        NEXT_PASS (pass_copy_prop, 8);
     in pass-instances.def lead to fields:
        opt_pass *pass_copy_prop_1;
        ...
        opt_pass *pass_copy_prop_8;  */

#define INSERT_PASSES_AFTER(PASS)
#define PUSH_INSERT_PASSES_WITHIN(PASS)
#define POP_INSERT_PASSES()
#define NEXT_PASS(PASS, NUM) opt_pass *PASS ## _ ## NUM
#define TERMINATE_PASS_LIST()

#include "pass-instances.def"

#undef INSERT_PASSES_AFTER
#undef PUSH_INSERT_PASSES_WITHIN
#undef POP_INSERT_PASSES
#undef NEXT_PASS
#undef TERMINATE_PASS_LIST

}; // class pass_manager

} // namespace gcc

#endif /* ! GCC_PASS_MANAGER_H */

