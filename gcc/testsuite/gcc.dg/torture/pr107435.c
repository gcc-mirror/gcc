/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

struct qlist_head {
  struct qlist_head *next;
};
void
qlist_add (struct qlist_head *new, struct qlist_head *head)
{
  struct qlist_head *prev = head;
  new->next = head->next;
  prev->next = new;
}
struct {
  struct qlist_head queue_link;
} free_list, prealloc[64];
void
dbpf_open_cache_initialize()
{
  int i = 0;
  for (; i < 64; i++)
    qlist_add(&prealloc[i].queue_link, &free_list.queue_link);
}
