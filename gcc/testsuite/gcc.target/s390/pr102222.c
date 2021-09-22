/* { dg-do compile } */
/* { dg-options "-O2 -m31 -mesa" } */

struct squashfs_reg_inode_header_1 read_inode_inode;

int read_inode_val;

struct squashfs_reg_inode_header_1
{
  int file_size:32;
} __attribute__((packed)) read_inode ();

void foo (void)
{
  read_inode_inode.file_size = read_inode_val;
}
