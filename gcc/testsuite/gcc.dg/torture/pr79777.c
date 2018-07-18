/* { dg-do compile } */

typedef unsigned short __u16;
#if __SIZEOF_INT__ < 4
  __extension__ typedef __UINT32_TYPE__ __u32;
  __extension__ typedef __UINT32_TYPE__ u32;
#else
  typedef unsigned int __u32;
  typedef unsigned int u32;
#endif
typedef unsigned char u8;
typedef __u16 __le16;
typedef __u32 __le32;
typedef u32 secno;
struct bplus_internal_node {
    __le32 file_secno;
    __le32 down;
};
struct bplus_header {
    u8 n_used_nodes;
    __le16 first_free;
    union {
	struct bplus_internal_node internal[0];
    }
    u;
};

__u16 __fswab16(__u16 val);
__u32 __fswab32(__u32 val);
void hpfs_ea_remove (__u32);

void hpfs_truncate_btree(secno f, int fno, unsigned secs, struct bplus_header *btree)
{
  int i, j;
  for (i = 0; i < btree->n_used_nodes; i++)
    if ((__builtin_constant_p((__u32)(( __u32)(__le32)(btree->u.internal[i].file_secno))) ? ((__u32)( (((__u32)(( __u32)(__le32)(btree->u.internal[i].file_secno)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__le32)(btree->u.internal[i].file_secno)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__le32)(btree->u.internal[i].file_secno)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__le32)(btree->u.internal[i].file_secno)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__le32)(btree->u.internal[i].file_secno))) >= secs) goto f;
  return;
f:
  for (j = i + 1; j < btree->n_used_nodes; j++)
    hpfs_ea_remove((__builtin_constant_p((__u32)(( __u32)(__le32)(btree->u.internal[j].down))) ? ((__u32)( (((__u32)(( __u32)(__le32)(btree->u.internal[j].down)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__le32)(btree->u.internal[j].down)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__le32)(btree->u.internal[j].down)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__le32)(btree->u.internal[j].down)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__le32)(btree->u.internal[j].down))));
  btree->n_used_nodes = i + 1;
  btree->first_free = (( __le16)(__builtin_constant_p((__u16)((8 + 8 * btree->n_used_nodes))) ? ((__u16)( (((__u16)((8 + 8 * btree->n_used_nodes)) & (__u16)0x00ffU) << 8) | (((__u16)((8 + 8 * btree->n_used_nodes)) & (__u16)0xff00U) >> 8))) : __fswab16((8 + 8 * btree->n_used_nodes))));
}
