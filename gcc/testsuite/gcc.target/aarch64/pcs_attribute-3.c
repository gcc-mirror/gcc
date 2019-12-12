/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-require-effective-target aarch64_variant_pcs } */

/* Test that .variant_pcs is emitted for vector PCS symbol references.  */

#define ATTR __attribute__ ((aarch64_vector_pcs))

static void f_local_basepcs (void)
{
}

static void (*f_ifunc_basepcs_resolver ()) (void)
{
  return (void (*)(void))f_local_basepcs;
}

__attribute__ ((ifunc ("f_ifunc_basepcs_resolver")))
void f_ifunc_basepcs (void);

ATTR static void f_local_vpcs (void)
{
}

static void (*f_ifunc_vpcs_resolver ()) (void)
{
  return (void (*)(void))f_local_vpcs;
}

__attribute__ ((ifunc ("f_ifunc_vpcs_resolver")))
ATTR void f_ifunc_vpcs (void);

__attribute__ ((visibility ("hidden")))
__attribute__ ((ifunc ("f_ifunc_vpcs_resolver")))
ATTR void f_hidden_ifunc_vpcs (void);

__attribute__ ((ifunc ("f_ifunc_vpcs_resolver")))
ATTR static void f_local_ifunc_vpcs (void);

void (*refs_basepcs[]) (void) = {
	f_ifunc_basepcs,
};

void (*ATTR refs_vpcs[]) (void) = {
	f_ifunc_vpcs,
	f_hidden_ifunc_vpcs,
	f_local_ifunc_vpcs,
};

/* Note: local symbols don't need .variant_pcs, but gcc generates it, so
   we check them here.  */

/* { dg-final { scan-assembler-not {\.variant_pcs\tf_local_basepcs} } } */
/* { dg-final { scan-assembler-not {\.variant_pcs\tf_ifunc_basepcs} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_local_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_ifunc_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_hidden_ifunc_vpcs} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\tf_local_ifunc_vpcs} 1 } } */
