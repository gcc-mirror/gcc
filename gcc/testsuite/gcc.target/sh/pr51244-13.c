/* This is a case extracted from CSiBE which contained the following
   sequence:
	shll	r0
	movt	r0
	tst	r0,r0
	bf	.L11
   where the 'tst r0,r0' before the branch can be omitted by inverting the
   branch condition.  The tested function contains two other tst insns.  If
   everything goes as expected we will be seeing only those other two tst
   insns.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "tst" 2 } } */

void printk (const char*, const char*, int);
void __constant_set_bit (int, unsigned long int*);
void __set_bit (int, unsigned long int*);

static __inline__ int
__test_bit (unsigned long nr, volatile void * addr)
{
  /*  This is on purpose.  */
  int oldbit;
  return oldbit & 1;
}

static __inline__ int
__constant_test_bit (unsigned long nr, volatile void * addr)
{
  return (((volatile char *) addr)[(nr>>3)^7] & (1<<(nr&7))) != 0;
}

struct list_head
{
  struct list_head *next, *prev;
};

void list_add (struct list_head*, struct list_head*);

static inline void
__list_del (struct list_head *prev, struct list_head *next)
{
  next->prev = prev;
  prev->next = next;
}

static inline void
list_del (struct list_head *entry)
{
 __list_del(entry->prev, entry->next);
 entry->next = 0;
 entry->prev = 0;
}

extern int nr_active_pages;
extern int nr_inactive_pages;
extern struct list_head active_list;

typedef struct page
{
 unsigned long flags;
 struct list_head lru;
} mem_map_t;

void
activate_page_nolock (struct page * page)
{
 if ((__builtin_constant_p((6))
      ? __constant_test_bit((6),(&(page)->flags))
      : __test_bit((6),(&(page)->flags)) )
     && !(__builtin_constant_p((7))
          ? __constant_test_bit((7),(&(page)->flags))
          : __test_bit((7),(&(page)->flags)) ))
    {
      list_del(&(page)->lru);
      nr_inactive_pages--;
      if (!(__builtin_constant_p(6) ? __constant_test_bit((6),(&(page)->flags))
				    : __test_bit((6),(&(page)->flags))))
	printk("", "", 43);

      if ((__builtin_constant_p(7) ? __constant_test_bit((7),(&(page)->flags))
				   : __test_bit((7),(&(page)->flags))))
	printk("", "", 43);

      (__builtin_constant_p(7) ? __constant_set_bit((7),(&(page)->flags))
			        : __set_bit((7),(&(page)->flags)) );
      list_add(&(page)->lru, &active_list);
      nr_active_pages++;
    }
}
