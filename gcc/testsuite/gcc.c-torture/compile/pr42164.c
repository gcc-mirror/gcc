typedef struct
{
  unsigned long long pte;
} pte_t;
pte_t mk_swap_pte (unsigned long offset)
{
  pte_t pte;
  pte.pte = (offset << 40);
  return pte;
}
int pte_file (pte_t pte)
{
  return pte.pte & (1 << 4);
}
typedef struct
{
  unsigned long val;
} swp_entry_t;
unsigned long swp_offset(swp_entry_t);
void __BUG_ON(unsigned long);
pte_t swp_entry_to_pte (swp_entry_t entry)
{
  swp_entry_t arch_entry;
  arch_entry = (swp_entry_t){mk_swap_pte (swp_offset (entry)).pte};
  __BUG_ON ((unsigned long) pte_file ((pte_t) {arch_entry.val}));
  return (pte_t) {arch_entry.val};
}
