/* { dg-do run } */
/* { dg-options "-std=gnu99 -Wall -Wextra -O1" } */ 

extern void *memset (void*, int, unsigned long);
extern void abort (void);

struct radix_tree_root {
	unsigned int height;
	struct radix_tree_node *rnode;
};

struct radix_tree_node {
	unsigned int count;
	void *slots[64];
	unsigned long tags[2][2];
};

struct radix_tree_path {
	struct radix_tree_node *node, **slot;
	int offset;
};

static unsigned long height_to_maxindex[7] =
{0, 63, 4095, 262143, 16777215, 1073741823, 4294967295};

static inline void tag_clear(struct radix_tree_node *node, int tag, int offset)
{
	int nr;
	volatile unsigned long *addr;
	int mask;
	
	nr = offset;
	addr = &node->tags[tag][0];

	addr += nr >> 5;
	mask = 1 << (nr & 0x1f);
	*addr &= ~mask;
}

void *radix_tree_tag_clear(struct radix_tree_root *root, unsigned long index, int tag)
{
	struct radix_tree_path path[7], *pathp = path;
	unsigned int height, shift;
	void *ret = 0;
	
	height = root->height;
	if (index > height_to_maxindex[height])
		goto out;
	
	shift = (height - 1) * 6;
	pathp->node = 0;
	pathp->slot = &root->rnode;
	
	while (height > 0) {
		int offset;
		
		if (*pathp->slot == 0)
			goto out;
		
		offset = (index >> shift) & (64-1);
		pathp[1].offset = offset;
		pathp[1].node = *pathp[0].slot;
		pathp[1].slot = (struct radix_tree_node **)
			(pathp[1].node->slots + offset);
		pathp++;
		shift -= 6;
		height--;
	}
	
	ret = *pathp[0].slot;
	if (ret == 0)
		goto out;
	
	do {
		int idx;
		
		tag_clear(pathp[0].node, tag, pathp[0].offset);
		for (idx = 0; idx < 2; idx++) {
			if (pathp[0].node->tags[tag][idx])
				goto out;
		}
		pathp--;
	} while (pathp[0].node);
out:
	return ret;
}

int main ()
{
	struct radix_tree_root r;
	struct radix_tree_node node;
	void *p = (void *) 0xdeadbeef;
	
  	r.height = 1;
	r.rnode = &node;
	
	memset (&node, 0, sizeof (node));
	
	node.count = 1;
	node.slots [13] = p;
	
	radix_tree_tag_clear (&r, 13, 1);
	
	if (r.rnode->slots[13] != p)
		abort ();
	return 0;
}
