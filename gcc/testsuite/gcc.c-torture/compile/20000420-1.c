struct z_candidate { struct z_candidate *next;int viable;};
int pedantic;

static struct z_candidate *
splice_viable (cands)
     struct z_candidate *cands;
{
  struct z_candidate **p = &cands;

  for (; *p; )
    {
      if (pedantic ? (*p)->viable == 1 : (*p)->viable)
        p = &((*p)->next);
      else
        *p = (*p)->next;
    }

  return cands;
}
