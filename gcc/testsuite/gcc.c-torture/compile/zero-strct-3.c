typedef struct {} spinlock_t;
struct sk_buff_head {
  int i;
  spinlock_t lock;
};
struct sk_buff_head audit_skb_queue;
void audit_init(void)
{
  struct sk_buff_head *list = &audit_skb_queue;
  spinlock_t a = {};
  audit_skb_queue.lock = a;
}
