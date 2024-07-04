#ifndef BPF_CORE_BUILTINS_H
#define BPF_CORE_BUILTINS_H

#include "btfext-out.h"

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

enum bpf_field_info_kind {
  BPF_FIELD_BYTE_OFFSET = 0,	/* field byte offset */
  BPF_FIELD_BYTE_SIZE = 1,
  BPF_FIELD_EXISTS = 2,		/* field existence in target kernel */
  BPF_FIELD_SIGNED = 3,
  BPF_FIELD_LSHIFT_U64 = 4,
  BPF_FIELD_RSHIFT_U64 = 5,
};

/* second argument to __builtin_btf_type_id () built-in */
enum bpf_type_id_kind {
  BPF_TYPE_ID_LOCAL = 0,		/* BTF type ID in local program */
  BPF_TYPE_ID_TARGET = 1,		/* BTF type ID in target kernel */
};

/* second argument to __builtin_preserve_type_info () built-in */
enum bpf_type_info_kind {
  BPF_TYPE_EXISTS = 0,		/* type existence in target kernel */
  BPF_TYPE_SIZE = 1,		/* type size in target kernel */
  BPF_TYPE_MATCHES = 2,		/* type match in target kernel */
};

/* second argument to __builtin_preserve_enum_value () built-in */
enum bpf_enum_value_kind {
  BPF_ENUMVAL_EXISTS = 0,		/* enum value existence in kernel */
  BPF_ENUMVAL_VALUE = 1,		/* enum value value relocation */
};

extern GTY (()) tree bpf_builtins[(int) BPF_BUILTIN_MAX];

void bpf_init_core_builtins (void);
rtx bpf_expand_core_builtin (tree exp, enum bpf_builtins code);
tree bpf_resolve_overloaded_core_builtin (location_t loc, tree fndecl,
					  void *arglist);
void
bpf_output_core_reloc (rtx *operands, int nr_ops);

#endif
