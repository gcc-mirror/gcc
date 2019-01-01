/* brig-to-generic.h -- brig to gcc generic conversion
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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

#ifndef BRIG_TO_GENERIC_H
#define BRIG_TO_GENERIC_H

#include <string>
#include <map>
#include <vector>

#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"
#include "opts.h"
#include "tree.h"
#include "tree-iterator.h"
#include "hsa-brig-format.h"
#include "brig-function.h"

struct reg_decl_index_entry;

/* Converts an HSAIL BRIG input to GENERIC.  This class holds global state
   for the translation process.  Handling of the smaller pieces of BRIG data
   is delegated to various handler classes declared in
   brig-code-entry-handlers.h.  */

class brig_to_generic
{
public:
  typedef std::map<const BrigDirectiveVariable *, tree> variable_index;

private:
  typedef std::map<std::string, size_t> var_offset_table;
  typedef std::map<const BrigBase *, std::string> name_index;

public:
  brig_to_generic ();

  void analyze (const char *brig_blob);
  void parse (const char *brig_blob);

  void write_globals ();

  std::string get_string (size_t entry_offset) const;

  const BrigData *get_brig_data_entry (size_t entry_offset) const;
  const BrigBase *get_brig_operand_entry (size_t entry_offset) const;
  const BrigBase *get_brig_code_entry (size_t entry_offset) const;

  void append_global (tree g);

  tree function_decl (const std::string &name);
  void add_function_decl (const std::string &name, tree func_decl);

  tree global_variable (const std::string &name) const;
  void add_global_variable (const std::string &name, tree var_decl);
  void add_host_def_var_ptr (const std::string &name, tree var_decl);
  void add_decl_call (tree call);

  void start_function (tree f);
  void finish_function ();

  void append_private_variable (const std::string &name, size_t size,
				size_t alignment);

  size_t
  private_variable_segment_offset (const std::string &name) const;

  bool
  has_private_variable (const std::string &name) const;

  size_t private_variable_size (const std::string &name) const;

  template <typename T>
    std::string
    get_mangled_name_tmpl (const T *brigVar) const;

  std::string get_mangled_name (const BrigDirectiveFbarrier *fbar) const
    { return get_mangled_name_tmpl (fbar); }
  std::string get_mangled_name (const BrigDirectiveVariable *var) const
    { return get_mangled_name_tmpl (var); }
  std::string get_mangled_name (const BrigDirectiveExecutable *func) const;

  size_t private_segment_size () const;

  brig_function *get_finished_function (tree func_decl);

  void add_group_variable (const std::string &name, size_t size,
			   size_t alignment, bool function_scope);

  void add_reg_used_as_type (const BrigOperandRegister &brig_reg,
			     tree operand_type);

  static tree s_fp16_type;
  static tree s_fp32_type;
  static tree s_fp64_type;

  /* The default rounding mode that should be used for float instructions.
     This can be set in each BRIG module header.  */
  BrigRound8_t m_default_float_rounding_mode;

  /* The currently built function.  */
  brig_function *m_cf;

  /* Stores the module and function scope group variable offsets.  */
  group_variable_offset_index m_module_group_variables;

  /* The name of the currently handled BRIG module.  */
  std::string m_module_name;

  /* Set to true if the compilation is in 'analyze' phase.  */
  bool m_analyzing;

  /* Accumulates the total group segment usage.  */
  size_t m_total_group_segment_usage;

  /* Statistics about register uses per function.  */
  std::map<std::string, regs_use_index> m_fn_regs_use_index;

private:

  void find_brig_sections ();
  /* The BRIG blob and its different sections of the file currently being
     parsed.  */
  const char *m_brig;
  const char *m_data;
  size_t m_data_size;
  const char *m_operand;
  size_t m_operand_size;
  const char *m_code;
  size_t m_code_size;

  tree m_globals;

  label_index m_global_variables;

  /* Calls to declarations to be fixed in the end of processing to call
     defs instead.  */
  std::vector<tree> m_decl_call;

  /* The size of each private variable, including the alignment padding.  */
  std::map<std::string, size_t> m_private_data_sizes;

  /* And private.  */
  size_t m_next_private_offset;
  var_offset_table m_private_offsets;

  /* Name index for declared functions.  */
  label_index m_function_index;

  /* Stores all processed kernels in order.  */
  std::vector<brig_function *> m_kernels;

  /* Stores all already processed functions from the translation unit
     for some interprocedural analysis.  */
  std::map<std::string, brig_function *> m_finished_functions;

  /* The original dump file.  */
  FILE *m_dump_file;

  /* The original dump file flags.  */
  dump_flags_t m_dump_flags;
};

/* Produce a "mangled name" for the given brig variable.  The mangling is used
   to make unique global symbol names for module and function scope variables.
   The templated version is suitable for most of the variable types.  Functions
   and kernels (BrigDirectiveExecutable) are handled with a specialized
   get_mangled_name() version.  */

template <typename T>
std::string
brig_to_generic::get_mangled_name_tmpl (const T *brigVar) const
{
  std::string var_name = get_string (brigVar->name).substr (1);

  /* Mangle the variable name using the function name and the module name
     in case of a function scope variable.  */
  if (m_cf != NULL
      && m_cf->has_function_scope_var (&brigVar->base))
    var_name = m_cf->m_name + "." + var_name;

  if (brigVar->linkage == BRIG_LINKAGE_MODULE)
    var_name = "gccbrig." + m_module_name + "." + var_name;
  return var_name;
}

/* An interface to organize the different types of BRIG element handlers.  */

class brig_entry_handler
{
public:
  brig_entry_handler (brig_to_generic &parent) : m_parent (parent)
  {
  }

  /* Handles the brig_code data at the given pointer and adds it to the
     currently built tree.  Returns the number of consumed bytes;  */
  virtual size_t operator () (const BrigBase *base) = 0;

protected:
  brig_to_generic &m_parent;
};

tree call_builtin (tree pdecl, int nargs, tree rettype, ...);

tree build_resize_convert_view (tree destination_type, tree source);
tree build_reinterpret_to_uint (tree source);

tree build_stmt (enum tree_code code, ...);

tree get_unsigned_int_type (tree type);

tree get_scalar_unsigned_int_type (tree type);
void set_externally_visible (tree decl);

void set_inline (tree decl);

void dump_function (FILE *dump_file, brig_function *f);

#endif
