/* { dg-do compile } */
/* { dg-options "-O3 -fprofile-generate" } */
/* { dg-require-profiling "-fprofile-generate" } */

typedef signed long long int __int64_t;
typedef unsigned long long int __uint64_t;
typedef __int64_t int64_t;
typedef __uint64_t uint64_t;
inline void
BLI_endian_switch_int64 (int64_t *val)
{
  uint64_t tval = *val;
  *val = ((tval >> 56)) | ((tval << 40) & 0x00ff000000000000ll)
	 | ((tval << 24) & 0x0000ff0000000000ll)
	 | ((tval << 8) & 0x000000ff00000000ll)
	 | ((tval >> 8) & 0x00000000ff000000ll)
	 | ((tval >> 24) & 0x0000000000ff0000ll)
	 | ((tval >> 40) & 0x000000000000ff00ll) | ((tval << 56));
}
typedef struct anim_index_entry
{
  unsigned long long seek_pos_dts;
  unsigned long long pts;
} anim_index_entry;
extern struct anim_index_entry *
MEM_callocN (int);
struct anim_index
{
  int num_entries;
  struct anim_index_entry *entries;
};
struct anim_index *
IMB_indexer_open (const char *name)
{
  char header[13];
  struct anim_index *idx;
  int i;
  idx->entries = MEM_callocN (8);
  if (((1 == 0) != (header[8] == 'V')))
    {
      for (i = 0; i < idx->num_entries; i++)
	{
	  BLI_endian_switch_int64 ((int64_t *) &idx->entries[i].seek_pos_dts);
	  BLI_endian_switch_int64 ((int64_t *) &idx->entries[i].pts);
	}
    }
}
