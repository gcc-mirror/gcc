
struct list_head {
 struct list_head *next, *prev;
};
struct ib_fmr {
 int *device;
 struct list_head list;
};
static inline
struct mthca_fmr *to_mfmr(struct ib_fmr *ibmr)
{
 const struct ib_fmr *__mptr = (ibmr);
 return (struct mthca_fmr *)( (char *)__mptr );
}
int mthca_is_memfree(void);
void mthca_arbel_fmr_unmap (struct mthca_fmr *);
void mthca_tavor_fmr_unmap (struct mthca_fmr *);
void mthca_unmap_fmr(struct list_head *fmr_list)
{
 struct ib_fmr *fmr;
 if (mthca_is_memfree())
 {
 for (fmr =
 ({ const struct list_head *__mptr = ((fmr_list)->next); (struct ib_fmr *)(
(char *)__mptr - 8 );});
 &fmr->list != (fmr_list);
 fmr = ({ const struct list_head *__mptr = (fmr->list.next); (struct ib_fmr
*)( (char *)__mptr - 8);})
 )
  mthca_arbel_fmr_unmap(to_mfmr(fmr));
 }
 else
 for (fmr =
 ({ const struct list_head *__mptr = ((fmr_list)->next); (struct ib_fmr *)(
(char *)__mptr - 8);});
  &fmr->list != (fmr_list);
  fmr = ({ const struct list_head *__mptr = (fmr->list.next); (struct ib_fmr
*)( (char *)__mptr - 8);})
  )
  mthca_tavor_fmr_unmap(to_mfmr(fmr));
}
