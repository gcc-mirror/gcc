/* { dg-additional-options "-O1" } */

struct list_head {
  struct list_head *next, *prev;
};

struct mbochs_dmabuf {
  /* [...snip...] */
  struct dma_buf *buf;
  /* [...snip...] */
  struct list_head next;
  /* [...snip...] */
};

void mbochs_close(struct list_head *dmabufs,
		  struct mbochs_dmabuf *dmabuf,
		  struct mbochs_dmabuf *tmp)
{
  /* [...snip...] */
  while (&dmabuf->next != dmabufs)
    {
      dmabuf = tmp;
      tmp = ((struct mbochs_dmabuf *)((void *)(tmp->next.next) - __builtin_offsetof(struct mbochs_dmabuf, next)));
    }

  /* [...snip...] */
}
