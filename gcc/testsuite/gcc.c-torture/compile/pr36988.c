typedef struct {
    unsigned char mbxCommand;
} MAILBOX_t;
void __writel(unsigned int);
void lpfc_sli_brdrestart(void)
{
  volatile unsigned int word0;
  MAILBOX_t *mb;
  mb = (MAILBOX_t *) &word0;
  mb->mbxCommand = 0x1A;
  __writel((*(unsigned int *) mb));
}
