#ifndef BPF_CORE_BUILTINS_H
#define BPF_CORE_BUILTINS_H

#include "coreout.h"

enum bpf_builtins
{
  BPF_BUILTIN_UNUSED = 0,
  /* Built-ins for non-generic loads and stores.  */
  BPF_BUILTIN_LOAD_BYTE,
  BPF_BUILTIN_LOAD_HALF,
  BPF_BUILTIN_LOAD_WORD,

  /* Compile Once - Run Everywhere (CO-RE) support.  */
  BPF_CORE_BUILTINS_MARKER = 10,
  BPF_BUILTIN_PRESERVE_ACCESS_INDEX,
  BPF_BUILTIN_PRESERVE_FIELD_INFO,
  BPF_BUILTIN_BTF_TYPE_ID,
  BPF_BUILTIN_PRESERVE_TYPE_INFO,
  BPF_BUILTIN_PRESERVE_ENUM_VALUE,

  /* CO-RE INTERNAL reloc.  */
  BPF_BUILTIN_CORE_RELOC,

  BPF_BUILTIN_MAX,
};

extern GTY (()) tree bpf_builtins[(int) BPF_BUILTIN_MAX];

void bpf_init_core_builtins (void);
rtx bpf_expand_core_builtin (tree exp, enum bpf_builtins code);
tree bpf_resolve_overloaded_core_builtin (location_t loc, tree fndecl,
					  void *arglist);

#endif
