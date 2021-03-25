#include <stdlib.h>

struct hashmap_entry {
	struct hashmap_entry *next;
	unsigned int hash;
};

struct strbuf {
	size_t alloc;
	size_t len;
	char *buf;
};

struct oid2strbuf {
	struct hashmap_entry ent; /* must be the first member! */
	unsigned char key[21];
	struct strbuf *value;
};


struct hashmap_iter {
	struct hashmap *map;
	struct hashmap_entry *next;
	unsigned int tablepos;
};

struct hashmap {
	struct hashmap_entry **table;
	// hashmap_cmp_fn cmpfn;
	unsigned int size, tablesize, grow_at, shrink_at;
	unsigned disallow_rehash : 1;
};
void strbuf_init(struct strbuf *, size_t);
void *hashmap_iter_next(struct hashmap_iter *iter);
void hashmap_free(struct hashmap *map, int free_entries);
void hashmap_iter_init(struct hashmap *map, struct hashmap_iter *iter);

void strbuf_release(struct strbuf *sb)
{
	if (sb->alloc) {  /* { dg-bogus "use after 'free'" } */
		free(sb->buf);
		strbuf_init(sb, 0);
	}
}

void oid2strbuf_free(struct hashmap *map) {
	struct hashmap_iter iter;
	struct hashmap_entry *e;

	hashmap_iter_init(map, &iter);
	while ((e = hashmap_iter_next(&iter))) {
		struct oid2strbuf *e_strbuf = (struct oid2strbuf *)e;
		strbuf_release(e_strbuf->value); /* { dg-bogus "use after 'free'" } */
		free(e_strbuf->value);
		free(e);
	}

	hashmap_free(map, 0);
}

