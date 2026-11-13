/* Reduced from erofs-utils-1.9.3 lib/inode.c erofs_iput(), which is:
   Copyright (C) 2018 HUAWEI, Inc. GPL-2.0+ OR MIT

   Intended as a regression test for a false -Wanalyzer-malloc-leak
   (PR analyzer/126832), but it also shows a false -Wanalyzer-out-of-bounds.
   { dg-do compile }
   { dg-additional-options "-O2 -Wno-analyzer-symbol-too-complex" }  */

typedef __SIZE_TYPE__ size_t;
extern void free(void *);
extern void *malloc(size_t);

#define container_of(ptr, type, member) ({			\
      const typeof(((type *)0)->member) *__mptr = (ptr);  /* { dg-bogus "Wanalyzer-out-of-bounds" "out-of-bounds" { xfail *-*-* } } */ \
	(type *)((char *)__mptr - __builtin_offsetof(type, member)); })

struct list_head {
	struct list_head *prev;
	struct list_head *next;
};

static inline void init_list_head(struct list_head *list) {
	list->prev = list;
	list->next = list;
}

static inline void list_add_tail(struct list_head *entry,
				 struct list_head *head) {
	entry->prev = head->prev;
	entry->next = head;
	head->prev->next = entry;
	head->prev = entry;
}

#define list_for_each_entry_safe(pos, n, head, member)                    \
	for (pos = container_of((head)->next, typeof(*pos), member),      \
	     n   = container_of(pos->member.next, typeof(*pos), member);  \
	     &pos->member != (head);                                      \
	     pos = n, n = container_of(n->member.next, typeof(*n), member))

struct item {
	int data;
	struct list_head link;
};

void free_all(struct list_head *head) {
	struct item *p, *n;
	list_for_each_entry_safe(p, n, head, link)
		free(p);
} /* { dg-bogus "leak" "PR analyzer/126832" } */

void test(void) {
	struct list_head head;
	struct item *a;

	init_list_head(&head);

	a = (struct item *)malloc(sizeof(*a));
	if (!a) return;
	list_add_tail(&a->link, &head);

	free_all(&head);
}
