void abort(void);
void exit(int);

struct inode {
	long long		i_size;
	struct super_block	*i_sb;
};

struct file {
	long long		f_pos;
};

struct super_block {
	int			s_blocksize;
	unsigned char		s_blocksize_bits;
	int			s_hs;
};

static char *
isofs_bread(unsigned int block)
{
	if (block)
	  abort ();
	exit(0);
}

static int
do_isofs_readdir(struct inode *inode, struct file *filp)
{
	int bufsize = inode->i_sb->s_blocksize;
	unsigned char bufbits = inode->i_sb->s_blocksize_bits;
	unsigned int block, offset;
	char *bh = 0;
	int hs;

 	if (filp->f_pos >= inode->i_size)
		return 0;
 
	offset = filp->f_pos & (bufsize - 1);
	block = filp->f_pos >> bufbits;
	hs = inode->i_sb->s_hs;

	while (filp->f_pos < inode->i_size) {
		if (!bh)
			bh = isofs_bread(block);

		hs += block << bufbits;

		if (hs == 0)
			filp->f_pos++;

		if (offset >= bufsize)
			offset &= bufsize - 1;

		if (*bh)
			filp->f_pos++;

		filp->f_pos++;
	}
	return 0;
}

struct super_block s;
struct inode i;
struct file f;

int
main(int argc, char **argv)
{
	s.s_blocksize = 512;
	s.s_blocksize_bits = 9;
	i.i_size = 2048;
	i.i_sb = &s;
	f.f_pos = 0;

	do_isofs_readdir(&i,&f);
	abort ();
}
