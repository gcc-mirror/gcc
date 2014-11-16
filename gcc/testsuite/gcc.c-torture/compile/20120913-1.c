struct list_head {
  struct list_head *next, *prev;
};
struct dm_exception {
  struct list_head hash_list;
  unsigned long long old_chunk;
  unsigned long long new_chunk;
};
struct dm_exception *dm_lookup_exception(struct list_head *table, unsigned long long chunk) {
  struct list_head *slot;
  struct dm_exception *e;
  slot = &table[0];
  e = (struct dm_exception *)slot->next;
  for (; &e->hash_list != (slot);)
    if (chunk <= (e->new_chunk>>56))
      return e;
}
