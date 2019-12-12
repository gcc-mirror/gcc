/* { dg-do compile } */
/* { dg-require-effective-target aarch64_variant_pcs } */

/* Test that .variant_pcs is emitted for vector PCS symbol references.  */

#define ATTR __attribute__ ((aarch64_vector_pcs))

void f_undef_basepcs (void);

void f_def_basepcs (void)
{
}

ATTR void f_undef_vpcs (void);

ATTR void f_def_vpcs (void)
{
}

__attribute__ ((alias ("f_def_vpcs")))
ATTR void f_alias_vpcs (void);

__attribute__ ((weak, alias ("f_def_vpcs")))
ATTR void f_weak_alias_vpcs (void);

__attribute__ ((weak))
ATTR void f_weak_undef_vpcs (void);

__attribute__ ((visibility ("protected")))
ATTR void f_protected_vpcs (void)
{
}

__attribute__ ((visibility ("hidden")))
ATTR void f_hidden_vpcs (void)
{
}

ATTR static void f_local_vpcs (void)
{
}

__attribute__((weakref ("f_undef_vpcs")))
ATTR static void f_local_weakref_undef_vpcs (void);

__attribute__((weakref ("f_hidden_vpcs")))
ATTR static void f_local_weakref_def_vpcs (void);

ATTR void bar_undef_vpcs (void) __asm__ ("f_undef_renamed_vpcs");

ATTR void bar_def_vpcs (void) __asm__ ("f_def_renamed_vpcs");
ATTR void bar_def_vpcs (void)
{
}

void (*refs_basepcs[]) (void) = {
	f_undef_basepcs,
	f_def_basepcs,
};

void (*ATTR refs_vpcs[]) (void) = {
	f_undef_vpcs,
	f_def_vpcs,
	f_alias_vpcs,
	f_weak_alias_vpcs,
	f_weak_undef_vpcs,
	f_protected_vpcs,
	f_hidden_vpcs,
	f_local_vpcs,
	f_local_weakref_undef_vpcs,
	f_local_weakref_def_vpcs,
	bar_undef_vpcs,
	bar_def_vpcs,
};

/* Note: local symbols don't need .variant_pcs, but gcc generates it, so
   we check them here.  An undefined weakref does not show up in the
   symbol table, only the target symbol, so it does not need .variant_pcs.  */

/* { dg-final { scan-assembler-not {\.variant_pcs\tf_undef_basepcs} } } */
/* { dg-final { scan-assembler-not {\.variant_pcs\tf_def_basepcs} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_undef_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_def_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_alias_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_weak_alias_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_weak_undef_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_protected_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_hidden_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_local_vpcs} 1 } } */
/* { dg-final { scan-assembler-not {\.variant_pcs\tf_local_weakref_undef_vpcs} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_local_weakref_def_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_undef_renamed_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_def_renamed_vpcs} 1 } } */
