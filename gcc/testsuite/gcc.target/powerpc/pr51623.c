/* PR target/51623 */
/* { dg-do compile { target { { powerpc*-*-linux* && ilp32 } || { powerpc-*-eabi* powerpc-*-rtems* } } } } */
/* { dg-options "-mrelocatable -ffreestanding" } */

/* This generated an error, since the compiler was calling
   unlikely_text_section_p in a context where it wasn't valid.  */

typedef long long loff_t;
typedef unsigned size_t;


struct mtd_info {
  unsigned writesize;
  unsigned oobsize;
  const char *name;
};

extern int strcmp(const char *,const char *);
extern int strncmp(const char *,const char *,size_t);
extern char * strchr(const char *,int);

struct cmd_tbl_s {
  char *name;
};


int printf(const char *fmt, ...) __attribute__ ((format (__printf__, 1, 2)));
int putc (int);
void* malloc(size_t);
void free(void*);

extern unsigned long simple_strtoul(const char *,char **,unsigned int);

extern int nand_curr_device;
extern struct mtd_info nand_info[];

extern void cmd_usage(struct cmd_tbl_s *);

static int nand_dump(struct mtd_info *nand, unsigned long off, int only_oob)
{
  int i;
  unsigned char *datbuf, *oobbuf, *p;

  datbuf = malloc(nand->writesize + nand->oobsize);
  oobbuf = malloc(nand->oobsize);
  off &= ~(nand->writesize - 1);

  printf("Page %08lx dump:\n", off);
  i = nand->writesize >> 4;
  p = datbuf;

  while (i--) {
    if (!only_oob)
      printf("\t%02x %02x %02x %02x %02x %02x %02x %02x"
	     "  %02x %02x %02x %02x %02x %02x %02x %02x\n",
	     p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7],
	     p[8], p[9], p[10], p[11], p[12], p[13], p[14],
	     p[15]);
    p += 16;
  }

  i = nand->oobsize >> 3;
  free(datbuf);
  free(oobbuf);

  return 0;
}

int do_nand(struct cmd_tbl_s * cmdtp, int flag, int argc, char *argv[])
{
  int dev;
  unsigned long  off;
  char *cmd, *s;
  struct mtd_info *nand;

  if (argc < 2)
    goto usage;

  cmd = argv[1];

  if (strcmp(cmd, "info") == 0) {
    putc('\n');
    return 0;
  }

  if (strcmp(cmd, "device") == 0) {
    if (argc < 3) {
      putc('\n');
    }
    dev = (int)simple_strtoul(argv[2], ((void *)0), 10);
    nand_curr_device = dev;
    return 0;
  }

  if (strcmp(cmd, "bad") != 0 && strcmp(cmd, "erase") != 0  )
    goto usage;
  
  if (nand_curr_device < 0 ) {
    return 1;
  }
  nand = &nand_info[nand_curr_device];

  if (strcmp(cmd, "erase") == 0 || strcmp(cmd, "scrub") == 0) {
    int clean = argc > 2 && !strcmp("clean", argv[2]);
    int scrub = !strcmp(cmd, "scrub");
    return 0;
  }

  if (strncmp(cmd, "dump", 4) == 0) {
    if (argc < 3)
      goto usage;

    s = strchr(cmd, '.');
    off = (int)simple_strtoul(argv[2], ((void *)0), 16);
    
    if (s != ((void *)0) && strcmp(s, ".oob") == 0)
      nand_dump(nand, off, 1);
    else
      nand_dump(nand, off, 0);
    
    return 0;
  }
usage:
  cmd_usage(cmdtp);
  return 1;
}

void *ptr = do_nand;
