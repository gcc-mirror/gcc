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
	unsigned long tags[2];
};

struct radix_tree_path {
	struct radix_tree_node *node, **slot;
	int offset;
};

void radix_tree_tag_clear(struct radix_tree_root *root, unsigned long index)
{
	struct radix_tree_path path[7], *pathp = path;
	unsigned int height, shift;
	volatile unsigned long *addr;
	
	height = root->height;
	
	shift = (height - 1) * 6;
	path[0].slot = &root->rnode;
	
	while (height > 0) {
		int offset;
		
		offset = (index >> shift) & (64-1);
		pathp[1].offset = offset;
		pathp[1].node = *pathp[0].slot;
		pathp[1].slot = (struct radix_tree_node **)
			(pathp[1].node->slots + offset);
		pathp++;
		shift -= 6;
		height--;
	}
	
	addr = &(pathp->node->tags[0]) + 1;
	*addr = 574;
}

struct radix_tree_root r;
struct radix_tree_node node;

int main ()
{
  	r.height = 1;
	r.rnode = &node;
	
	memset (&node, 0, sizeof (node));
	
	node.count = 1;
	
	radix_tree_tag_clear (&r, 13);
	return 0;
}
