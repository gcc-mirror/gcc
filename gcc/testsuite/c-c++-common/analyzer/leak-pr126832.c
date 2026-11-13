/* Verify that we don't emit a false positive about a leak of "a"
   when tracking insertion into a intrusive linked list (PR analyzer/126832).
   Reduced from erofs-utils-1.9.3 erofs_iput().  */

typedef __SIZE_TYPE__ size_t;

struct list_head {
  struct list_head *prev;
  struct list_head *next;
};

struct item {
  struct list_head link;
  int data;
};

void
test (void) {
  struct list_head head;
  struct item *a;

  head.prev = &head;
  head.next = &head;

  a = (struct item *)__builtin_malloc(sizeof(*a));
  if (!a) return;

  a->link.prev = head.prev;
  a->link.next = &head;
  head.prev->next = &a->link;
  head.prev = &a->link;

  __builtin_free(head.next);
} /* { dg-bogus "leak of 'a'" } */
