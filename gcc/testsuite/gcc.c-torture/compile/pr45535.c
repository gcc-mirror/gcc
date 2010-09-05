typedef struct {
    unsigned long pmd0;
    unsigned long pmd1;
} pmd_t;
typedef unsigned int pgd_t;
struct mm_struct {
    pgd_t * pgd;
};
extern inline int pmd_bad(pmd_t pmd)
{
}
extern inline void pmd_clear(pmd_t * pmdp)
{
  ((*pmdp).pmd0) = 0x20 | 0x00;
  ((*pmdp).pmd1) = 0x20 | 0x00;
}
static inline void free_one_pmd(pmd_t * dir)
{
  if (pmd_bad(*dir)) {
      pmd_clear(dir);
  }
}
static inline void free_one_pgd(pgd_t * dir)
{
  int j;
  pmd_t * pmd;
  pmd = ((pmd_t *) ((unsigned long) (void *)(__pgd_val(dir) & (~((1UL << 12)-1)))) + (((0) >> 21) & (512 - 1)));
  for (j = 0; j < 512 ; j++) {
      free_one_pmd(pmd+j);
  }
}
void clear_page_tables(struct mm_struct *mm, unsigned long first, int nr)
{
  pgd_t * page_dir = mm->pgd;
  do {
      free_one_pgd(page_dir);
  } while (--nr);
}
