/* PR rtl-optimization/45593 */
/* Testcase by Arnaud Lacombe <lacombar@gmail.com> */

typedef unsigned int __u32;
typedef __u32 __be32;
static inline __attribute__((always_inline)) int __attribute__((__cold__)) printk(const char *s, ...) { return 0; }
typedef struct journal_s journal_t;
typedef struct journal_header_s
{
 __be32 h_magic;
 __be32 h_blocktype;
 __be32 h_sequence;
} journal_header_t;
typedef struct journal_superblock_s
{
 journal_header_t s_header;
 __be32 s_blocksize;
 __be32 s_maxlen;
} journal_superblock_t;
struct journal_s
{
 struct buffer_head *j_sb_buffer;
 journal_superblock_t *j_superblock;
 int j_format_version;
 int j_blocksize;
 unsigned int j_maxlen;
};
static void journal_fail_superblock (journal_t *journal)
{
 journal->j_sb_buffer = ((void *)0);
}
static int journal_get_superblock(journal_t *journal)
{
 struct buffer_head *bh;
 journal_superblock_t *sb;
 int err = -100;
 bh = journal->j_sb_buffer;
 if (!buffer_uptodate(bh)) {
  if (!buffer_uptodate(bh)) {
   printk ("JBD: IO error reading journal superblock\n");
   goto out;
  }
 }
 err = -101;
 if (sb->s_header.h_magic != (( __be32)(__u32)(0)) ||
     sb->s_blocksize != (( __be32)(__u32)(journal->j_blocksize))) {
  printk("JBD: no valid journal superblock found\n");
  goto out;
 }
 switch((( __u32)(__be32)(sb->s_header.h_blocktype))) {
 case 0:
 case 1:
  break;
 default:
  goto out;
 }
 if ((( __u32)(__be32)(sb->s_maxlen)) < journal->j_maxlen)
  journal->j_maxlen = (( __u32)(__be32)(sb->s_maxlen));
 else if ((( __u32)(__be32)(sb->s_maxlen)) > journal->j_maxlen) {
  printk ("JBD: journal file too short\n");
  goto out;
 }
 return 0;
out:
 journal_fail_superblock(journal);
 return err;
}
static int load_superblock(journal_t *journal)
{
 journal_get_superblock(journal);
 return 0;
}
int jbd2_journal_update_format (journal_t *journal)
{
 journal_get_superblock(journal);
 return 0;
}
int jbd2_journal_wipe(journal_t *journal, int write)
{
 load_superblock(journal);
 return 0;
}
