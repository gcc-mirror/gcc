/* Test that BTF type tags do not interfere with CO-RE relocations.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -dA -mco-re" } */

struct bpf_cpumask {
  int i;
  char c;
} __attribute__((preserve_access_index));

struct kptr_nested {
	struct bpf_cpumask * __attribute__((btf_type_tag("kptr"))) mask;
} __attribute__((preserve_access_index));

void foo (struct kptr_nested *nested)
{
  if (nested && nested->mask)
    nested->mask->i = 5;
}

/* { dg-final { scan-assembler-times "bpfcr_insn" 3 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct" 3 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:0\"\\)" 3 } } */
