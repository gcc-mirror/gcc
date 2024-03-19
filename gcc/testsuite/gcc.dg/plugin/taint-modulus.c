/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

/* Reduced from a -Wanalyzer-tainted-array-index false +ve
   seen in the Linux kernel's sound/drivers/opl3/opl3_synth.c.  */

extern unsigned long
copy_from_user(void* to, const void* from, unsigned long n);

struct sbi_patch
{
  unsigned char prog;
  unsigned char bank;
};
struct fm_patch
{
  unsigned char prog;
  unsigned char bank;
  struct fm_patch* next;
};
struct snd_opl3
{
  struct fm_patch* patch_table[32];
};
int
snd_opl3_load_patch(struct snd_opl3* opl3,
                    int prog,
                    int bank);
struct fm_patch*
snd_opl3_find_patch(struct snd_opl3* opl3,
                    int prog,
                    int bank,
                    int create_patch);
long
snd_opl3_write(struct snd_opl3* opl3,
               const char* buf,
               long count)
{
  long result = 0;
  int err = 0;
  struct sbi_patch inst;
  while (count >= sizeof(inst)) {
    if (copy_from_user(&inst, buf, sizeof(inst)))
      return -14;
    err = snd_opl3_load_patch(opl3, inst.prog, inst.bank);
    if (err < 0)
      break;
    result += sizeof(inst);
    count -= sizeof(inst);
  }
  return result > 0 ? result : err;
}
int
snd_opl3_load_patch(struct snd_opl3* opl3,
                    int prog,
                    int bank)
{
  struct fm_patch* patch;
  patch = snd_opl3_find_patch(opl3, prog, bank, 1);
  if (!patch)
    return -12;
  return 0;
}
struct fm_patch*
snd_opl3_find_patch(struct snd_opl3* opl3, int prog, int bank, int create_patch)
{
  unsigned int key = (prog + bank) % 32;
  struct fm_patch* patch;
  for (patch = opl3->patch_table[key]; patch; patch = patch->next) { /* { dg-bogus "use of attacker-controlled value in array lookup" } */
    if (patch->prog == prog && patch->bank == bank)
      return patch;
  }
  return ((void*)0);
}
