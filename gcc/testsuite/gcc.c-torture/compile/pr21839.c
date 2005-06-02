 typedef struct { } spinlock_t;
typedef struct {
 unsigned sequence;
 spinlock_t lock;
} seqlock_t;
void ext3_new_inode(seqlock_t *rsv_seqlock)
{
 *rsv_seqlock = (seqlock_t) { 0, (spinlock_t) { } };

}


