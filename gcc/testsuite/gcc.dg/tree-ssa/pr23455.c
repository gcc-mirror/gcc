/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
unsigned long outcnt;
extern void flush_outbuf(void);

void
bi_windup(unsigned int *outbuf, unsigned int bi_buf)
{
    unsigned long t1 = outcnt;
    outbuf[t1] = bi_buf;

    unsigned long t2 = outcnt;
    if (t2 == 16384)
      flush_outbuf();

    unsigned long t3 = outcnt;
    outbuf[t3] = bi_buf;
}
/* We should eliminate one load of outcnt, which will in turn let us eliminate
   one multiply of outcnt which will in turn let us eliminate
   one add involving outcnt and outbuf.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 3" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
