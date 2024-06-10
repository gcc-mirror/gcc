/* Test special meaning of .maps section for BTF when pruning.  For global
   variables of struct type placed in this section, we must treat members as
   though they are used directly, always collecting pointee types.
   Therefore, full type information for struct keep_me should be emitted.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -gprune-btf -dA" } */

/* { dg-final { scan-assembler-not "BTF_KIND_FWD 'keep_me'" } } */
/* { dg-final { scan-assembler "BTF_KIND_STRUCT 'keep_me'" } } */

struct keep_me {
  int a;
  char c;
};

struct {
  int *key;
  struct keep_me *value;
} my_map __attribute__((section (".maps")));
