typedef union { int ioport; volatile char *maddr; } bus_space_handle_t;
struct scb { unsigned short *hscb; };
struct ahd_softc
{
  int tags[2];
  bus_space_handle_t bshs[2];
  int dst_mode;
  int src_mode;
  int flags;
};
void outb(int, int);

int foo_inb(struct ahd_softc*);
int foo_int_int (int, int);
int ahd_inb(struct ahd_softc*);
int ahd_scb_active_in_fifo (void);

void ahd_flush_qoutfifo (struct ahd_softc *ahd, struct scb *scb)
{
  int src, dst, *a = &src, *b = &dst; *a = 1, *b = 1;
  int bb, p;

  if (ahd->src_mode == 1)
    { int src, dst, *a = &src, *b = &dst; *a = 1, *b = 1;}
  foo_int_int (ahd->src_mode, ahd->dst_mode);
  p = 1;
  if (ahd->src_mode == 2 && ahd->dst_mode == p)
    {
      if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
        outb (1, ahd->bshs[0].ioport );
      if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
        outb (1, ahd->bshs[0].ioport );
    }
  ahd->src_mode = 1;
  ahd->dst_mode = 2;
  while ((ahd_inb (ahd) & 0x01) != 0)
  {
    p = 1;
    if (ahd->src_mode == 2 && ahd->dst_mode == p)
      {
        if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
          outb (1, ahd->bshs[0].ioport );
        if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
          outb (1, ahd->bshs[0].ioport );
      }
    ahd->src_mode = 1;
    ahd->dst_mode = 2;
    if (ahd_scb_active_in_fifo () == 0)
      continue;
    p = 1;
    if (ahd->src_mode == 2 && ahd->dst_mode == p)
      {
        if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
          outb (1, ahd->bshs[0].ioport );
        if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
          outb (1, ahd->bshs[0].ioport );
      }
    ahd->src_mode = 1;
    ahd->dst_mode = 2;
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if ((ahd->flags & 1) != 0)
      foo_inb (ahd);
    if ((ahd->flags & 1) != 0)
      foo_inb (ahd);
    if ((ahd->flags & 1) != 0)
      foo_inb (ahd);
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if ((ahd->flags & 1) != 0)
      foo_inb (ahd);
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    bb = (*(scb->hscb));
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    bb = (*(scb->hscb));
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
    if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
      outb (1, ahd->bshs[0].ioport );
  }
  if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
    outb (1, ahd->bshs[0].ioport );
  if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
    outb (1, ahd->bshs[0].ioport );
  p = 1;
  if (ahd->src_mode == 2 && ahd->dst_mode == p)
    {
      if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
        outb (1, ahd->bshs[0].ioport );
      if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
        outb (1, ahd->bshs[0].ioport );
    }
  ahd->src_mode = 1;
  ahd->dst_mode = 2;
  if (ahd->src_mode == 2 && ahd->dst_mode == dst)
    {
      if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
        outb (1, ahd->bshs[0].ioport );
      if (ahd->tags[0] == 1) *(ahd->bshs[0].maddr);
        outb (1, ahd->bshs[0].ioport );
    }
  ahd->src_mode = 1;
  ahd->dst_mode = 2;
  ahd->flags |= 1;
}
