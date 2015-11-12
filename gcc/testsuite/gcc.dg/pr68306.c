/* { dg-do compile } */
/* { dg-options "-O3" } */

enum powerpc_pmc_type { PPC_PMC_IBM };
struct {
    unsigned num_pmcs;
    enum powerpc_pmc_type pmc_type;
} a;
enum powerpc_pmc_type b;
void fn1() { a.num_pmcs = a.pmc_type = b; }
