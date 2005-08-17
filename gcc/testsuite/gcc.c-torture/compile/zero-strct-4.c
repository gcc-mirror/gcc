typedef struct {} raw_spinlock_t;
typedef struct {
  raw_spinlock_t raw_lock;
} spinlock_t;
struct sk_buff_head {
  int i;
  spinlock_t lock;
};
struct sk_buff_head audit_skb_queue;
void audit_init(void)
{
  struct sk_buff_head *list = &audit_skb_queue;
  audit_skb_queue.lock = (spinlock_t) { .raw_lock = { } };
}
