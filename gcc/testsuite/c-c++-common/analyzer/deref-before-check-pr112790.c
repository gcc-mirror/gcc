/* Reproducer for false positive from -Wanalyzer-deref-before-check
   seen on Linux kernel's block/bdev.c due to -fanalyzer mishandling
   inlined functions.  */

/* { dg-additional-options "-O2 -g -fno-delete-null-pointer-checks" } */

typedef unsigned char u8;
struct inode {
  void *i_mapping;
  u8 i_blkbits;
};
struct block_device {
  struct inode *bd_inode;
};
int sync_blockdev(struct block_device *bdev);
int set_blocksize(struct block_device *bdev, u8 size) {
  if (bdev->bd_inode->i_blkbits != size) { /* { dg-bogus "pointer 'bdev' is dereferenced here" } */
    sync_blockdev(bdev);
  }
  return 0;
}
extern int filemap_write_and_wait(void *);
int sync_blockdev(struct block_device *bdev) {
  if (!bdev) /* { dg-bogus "check of 'bdev' for NULL after already dereferencing it" } */
    return 0;
  return filemap_write_and_wait(bdev->bd_inode->i_mapping);
}
