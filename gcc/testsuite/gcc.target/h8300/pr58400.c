/* { dg-do compile }  */
/* { dg-options "-Os -mh -mint32 -w -fpermissive" }  */

 typedef unsigned short __u16;
 typedef __signed__ int __s32;
 typedef unsigned int __u32;
 typedef unsigned char u8;
 typedef unsigned int u32;
 typedef signed long long s64;
 typedef unsigned long long u64;
 typedef long __kernel_long_t;
 typedef unsigned int __kernel_uid32_t;
 typedef unsigned int __kernel_gid32_t;
 typedef long long __kernel_loff_t;
 typedef __kernel_long_t __kernel_time_t;
 typedef __u32 __kernel_dev_t;
 typedef __kernel_dev_t dev_t;
 typedef unsigned short umode_t;
 typedef __kernel_uid32_t uid_t;
 typedef __kernel_gid32_t gid_t;
 typedef __kernel_loff_t loff_t;
 typedef u64 blkcnt_t;
 typedef unsigned gfp_t;
 typedef unsigned fmode_t;
 typedef struct {  int counter; }
 atomic_t;
 struct list_head {  struct list_head *next, *prev; };
 struct hlist_head {  struct hlist_node *first; };
 struct hlist_node {  struct hlist_node *next, **pprev; };
 struct callback_head {  struct callback_head *next;  void (*func)(struct callback_head *head); };
 static __inline__ __attribute__((always_inline)) __attribute__((no_instrument_function)) void set_bit(int nr, volatile unsigned long* addr) { volatile unsigned char *b_addr; b_addr = (volatile unsigned char *)addr + ((nr >> 3) ^ 3); if (__builtin_constant_p(nr)) { switch(nr & 7) { case 0: __asm__("bset" " #" "0" ",@%0"::"r"(b_addr):"memory"); break; case 1: __asm__("bset" " #" "1" ",@%0"::"r"(b_addr):"memory"); break; case 2: __asm__("bset" " #" "2" ",@%0"::"r"(b_addr):"memory"); break; case 3: __asm__("bset" " #" "3" ",@%0"::"r"(b_addr):"memory"); break; case 4: __asm__("bset" " #" "4" ",@%0"::"r"(b_addr):"memory"); break; case 5: __asm__("bset" " #" "5" ",@%0"::"r"(b_addr):"memory"); break; case 6: __asm__("bset" " #" "6" ",@%0"::"r"(b_addr):"memory"); break; case 7: __asm__("bset" " #" "7" ",@%0"::"r"(b_addr):"memory"); break; } } else { __asm__("bset" " %w0,@%1"::"r"(nr),"r"(b_addr):"memory"); } }
 static __inline__ __attribute__((always_inline)) __attribute__((no_instrument_function)) int test_bit(int nr, const unsigned long* addr) {  return (*((volatile unsigned char *)addr +                ((nr >> 3) ^ 3)) & (1UL << (nr & 7))) != 0; }
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) int test_bit_le(int nr, const void *addr) {  return test_bit(nr ^ ((32 -1) & ~0x7), addr); }
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) void __set_bit_le(int nr, void *addr) {  set_bit((nr ^ ((32 -1) & ~0x7)),(addr)); }
 typedef struct {  volatile unsigned int slock; }
 arch_spinlock_t;
 typedef struct raw_spinlock {  arch_spinlock_t raw_lock;  unsigned int magic, owner_cpu;  void *owner; }
 raw_spinlock_t;
 typedef struct spinlock {  union {   struct raw_spinlock rlock;  }; }
 spinlock_t;
 typedef atomic_t atomic_long_t;
 typedef struct {  long long counter; }
 atomic64_t;
 struct __wait_queue_head {  spinlock_t lock;  struct list_head task_list; };
 typedef struct __wait_queue_head wait_queue_head_t;
 struct completion {  unsigned int done;  wait_queue_head_t wait; };
 struct hlist_bl_head {  struct hlist_bl_node *first; };
 struct timespec {  __kernel_time_t tv_sec;  long tv_nsec; };
 typedef struct {  uid_t val; }
 kuid_t;
 typedef struct {  gid_t val; }
 kgid_t;
 typedef struct { unsigned long bits[((((1 << 0)) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; }
 nodemask_t;
 struct list_lru {  struct list_lru_node *node;  nodemask_t active_nodes; };
 struct radix_tree_root {  unsigned int height;  gfp_t gfp_mask;  struct radix_tree_node *rnode; };
 struct rb_root {  struct rb_node *rb_node; };
 struct mutex {  atomic_t count;  spinlock_t wait_lock;  struct list_head wait_list;  struct task_struct *owner;  const char *name;  void *magic; };
 struct shrinker {  unsigned long (*count_objects)(struct shrinker *,            struct shrink_control *sc);  unsigned long (*scan_objects)(struct shrinker *,           struct shrink_control *sc);  int seeks;  long batch;  unsigned long flags;  struct list_head list;  atomic_long_t *nr_deferred; };
 struct rw_semaphore {  __s32 activity;  raw_spinlock_t wait_lock;  struct list_head wait_list; };
 struct percpu_counter {  s64 count; };
 typedef long long qsize_t;
 struct mem_dqinfo {  struct quota_format_type *dqi_format;  int dqi_fmt_id;  struct list_head dqi_dirty_list;  unsigned long dqi_flags;  unsigned int dqi_bgrace;  unsigned int dqi_igrace;  qsize_t dqi_maxblimit;  qsize_t dqi_maxilimit;  void *dqi_priv; };
 struct quota_info {  unsigned int flags;  struct mutex dqio_mutex;  struct mutex dqonoff_mutex;  struct rw_semaphore dqptr_sem;  struct inode *files[2];  struct mem_dqinfo info[2];  const struct quota_format_ops *ops[2]; };
 struct address_space {  struct inode *host;  struct radix_tree_root page_tree;  spinlock_t tree_lock;  unsigned int i_mmap_writable;  struct rb_root i_mmap;  struct list_head i_mmap_nonlinear;  struct mutex i_mmap_mutex;  unsigned long nrpages;  unsigned long writeback_index;  const struct address_space_operations *a_ops;  unsigned long flags;  struct backing_dev_info *backing_dev_info;  spinlock_t private_lock;  struct list_head private_list;  void *private_data; }
 __attribute__((aligned(sizeof(long))));
 struct inode {  umode_t i_mode;  unsigned short i_opflags;  kuid_t i_uid;  kgid_t i_gid;  unsigned int i_flags;  struct posix_acl *i_acl;  struct posix_acl *i_default_acl;  const struct inode_operations *i_op;  struct super_block *i_sb;  struct address_space *i_mapping;  void *i_security;  unsigned long i_ino;  union {   const unsigned int i_nlink;   unsigned int __i_nlink;  };  dev_t i_rdev;  loff_t i_size;  struct timespec i_atime;  struct timespec i_mtime;  struct timespec i_ctime;  spinlock_t i_lock;  unsigned short i_bytes;  unsigned int i_blkbits;  blkcnt_t i_blocks;  unsigned long i_state;  struct mutex i_mutex;  unsigned long dirtied_when;  struct hlist_node i_hash;  struct list_head i_wb_list;  struct list_head i_lru;  struct list_head i_sb_list;  union {   struct hlist_head i_dentry;   struct callback_head i_rcu;  };  u64 i_version;  atomic_t i_count;  atomic_t i_dio_count;  atomic_t i_writecount;  const struct file_operations *i_fop;  struct file_lock *i_flock;  struct address_space i_data;  struct dquot *i_dquot[2];  struct list_head i_devices;  union {   struct pipe_inode_info *i_pipe;   struct block_device *i_bdev;   struct cdev *i_cdev;  };  __u32 i_generation;  __u32 i_fsnotify_mask;  struct hlist_head i_fsnotify_marks;  atomic_t i_readcount;  void *i_private; };
 enum {  SB_UNFROZEN = 0,  SB_FREEZE_WRITE = 1,  SB_FREEZE_PAGEFAULT = 2,  SB_FREEZE_FS = 3,  SB_FREEZE_COMPLETE = 4, };
 struct sb_writers {  struct percpu_counter counter[(SB_FREEZE_COMPLETE - 1)];  wait_queue_head_t wait;  int frozen;  wait_queue_head_t wait_unfrozen; };
 struct super_block {  struct list_head s_list;  dev_t s_dev;  unsigned char s_blocksize_bits;  unsigned long s_blocksize;  loff_t s_maxbytes;  struct file_system_type *s_type;  const struct super_operations *s_op;  const struct dquot_operations *dq_op;  const struct quotactl_ops *s_qcop;  const struct export_operations *s_export_op;  unsigned long s_flags;  unsigned long s_magic;  struct dentry *s_root;  struct rw_semaphore s_umount;  int s_count;  atomic_t s_active;  void *s_security;  const struct xattr_handler **s_xattr;  struct list_head s_inodes;  struct hlist_bl_head s_anon;  struct list_head s_files;  struct list_head s_mounts;  struct block_device *s_bdev;  struct backing_dev_info *s_bdi;  struct mtd_info *s_mtd;  struct hlist_node s_instances;  struct quota_info s_dquot;  struct sb_writers s_writers;  char s_id[32];  u8 s_uuid[16];  void *s_fs_info;  unsigned int s_max_links;  fmode_t s_mode;  u32 s_time_gran;  struct mutex s_vfs_rename_mutex;  char *s_subtype;  char *s_options;  const struct dentry_operations *s_d_op;  int cleancache_poolid;  struct shrinker s_shrink;  atomic_long_t s_remove_count;  int s_readonly_remount;  struct list_lru s_dentry_lru ;  struct list_lru s_inode_lru ; };
 struct timer_list {  struct list_head entry;  unsigned long expires;  struct tvec_base *base;  void (*function)(unsigned long);  unsigned long data;  int slack;  int start_pid;  void *start_site;  char start_comm[16]; };
 typedef void (*work_func_t)(struct work_struct *work);
 struct work_struct {  atomic_long_t data;  struct list_head entry;  work_func_t func; };
 struct delayed_work {  struct work_struct work;  struct timer_list timer;  struct workqueue_struct *wq;  int cpu; };
 struct kref {  atomic_t refcount; };
 struct kobject {  const char *name;  struct list_head entry;  struct kobject *parent;  struct kset *kset;  struct kobj_type *ktype;  struct sysfs_dirent *sd;  struct kref kref;  struct delayed_work release;  unsigned int state_initialized:1;  unsigned int state_in_sysfs:1;  unsigned int state_add_uevent_sent:1;  unsigned int state_remove_uevent_sent:1;  unsigned int uevent_suppress:1; };
 typedef int ext4_grpblk_t;
 typedef unsigned long long ext4_fsblk_t;
 typedef unsigned int ext4_group_t;
 struct ext4_sb_info {  unsigned long s_desc_size;  unsigned long s_inodes_per_block;  unsigned long s_blocks_per_group;  unsigned long s_clusters_per_group;  unsigned long s_inodes_per_group;  unsigned long s_itb_per_group;  unsigned long s_gdb_count;  unsigned long s_desc_per_block;  ext4_group_t s_groups_count;  ext4_group_t s_blockfile_groups;  unsigned long s_overhead;  unsigned int s_cluster_ratio;  unsigned int s_cluster_bits;  loff_t s_bitmap_maxbytes;  struct buffer_head * s_sbh;  struct ext4_super_block *s_es;  struct buffer_head **s_group_desc;  unsigned int s_mount_opt;  unsigned int s_mount_opt2;  unsigned int s_mount_flags;  unsigned int s_def_mount_opt;  ext4_fsblk_t s_sb_block;  atomic64_t s_resv_clusters;  kuid_t s_resuid;  kgid_t s_resgid;  unsigned short s_mount_state;  unsigned short s_pad;  int s_addr_per_block_bits;  int s_desc_per_block_bits;  int s_inode_size;  int s_first_ino;  unsigned int s_inode_readahead_blks;  unsigned int s_inode_goal;  spinlock_t s_next_gen_lock;  u32 s_next_generation;  u32 s_hash_seed[4];  int s_def_hash_version;  int s_hash_unsigned;  struct percpu_counter s_freeclusters_counter;  struct percpu_counter s_freeinodes_counter;  struct percpu_counter s_dirs_counter;  struct percpu_counter s_dirtyclusters_counter;  struct blockgroup_lock *s_blockgroup_lock;  struct proc_dir_entry *s_proc;  struct kobject s_kobj;  struct completion s_kobj_unregister;  struct super_block *s_sb;  struct journal_s *s_journal;  struct list_head s_orphan;  struct mutex s_orphan_lock;  unsigned long s_resize_flags;  unsigned long s_commit_interval;  u32 s_max_batch_time;  u32 s_min_batch_time;  struct block_device *journal_bdev;  char *s_qf_names[2];  int s_jquota_fmt;  unsigned int s_want_extra_isize;  struct rb_root system_blks;  struct ext4_group_info ***s_group_info;  struct inode *s_buddy_cache;  spinlock_t s_md_lock;  unsigned short *s_mb_offsets;  unsigned int *s_mb_maxs;  unsigned int s_group_info_size;  unsigned long s_stripe;  unsigned int s_mb_stream_request;  unsigned int s_mb_max_to_scan;  unsigned int s_mb_min_to_scan;  unsigned int s_mb_stats;  unsigned int s_mb_order2_reqs;  unsigned int s_mb_group_prealloc;  unsigned int s_max_dir_size_kb;  unsigned long s_mb_last_group;  unsigned long s_mb_last_start;  atomic_t s_bal_reqs;  atomic_t s_bal_success;  atomic_t s_bal_allocated;  atomic_t s_bal_ex_scanned;  atomic_t s_bal_goals;  atomic_t s_bal_breaks;  atomic_t s_bal_2orders;  spinlock_t s_bal_lock;  unsigned long s_mb_buddies_generated;  unsigned long long s_mb_generation_time;  atomic_t s_mb_lost_chunks;  atomic_t s_mb_preallocated;  atomic_t s_mb_discarded;  atomic_t s_lock_busy;  struct ext4_locality_group *s_locality_groups;  unsigned long s_sectors_written_start;  u64 s_kbytes_written;  unsigned int s_extent_max_zeroout_kb;  unsigned int s_log_groups_per_flex;  struct flex_groups *s_flex_groups;  ext4_group_t s_flex_groups_allocated;  struct workqueue_struct *unrsv_conversion_wq;  struct workqueue_struct *rsv_conversion_wq;  struct timer_list s_err_report;  struct ext4_li_request *s_li_request;  unsigned int s_li_wait_mult;  struct task_struct *s_mmp_tsk;  atomic_t s_last_trim_minblks;  struct crypto_shash *s_chksum_driver;  __u32 s_csum_seed;  struct shrinker s_es_shrinker;  struct list_head s_es_lru;  unsigned long s_es_last_sorted;  struct percpu_counter s_extent_cache_cnt;  spinlock_t s_es_lru_lock ; };
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) struct ext4_sb_info *EXT4_SB(struct super_block *sb) {  return sb->s_fs_info; }
 struct ext4_group_info {  unsigned long bb_state;  struct rb_root bb_free_root;  ext4_grpblk_t bb_first_free;  ext4_grpblk_t bb_free;  ext4_grpblk_t bb_fragments;  ext4_grpblk_t bb_largest_free_order;  struct list_head bb_prealloc_list;  struct rw_semaphore alloc_sem;  ext4_grpblk_t bb_counters[]; };
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) spinlock_t *ext4_group_lock_ptr(struct super_block *sb,            ext4_group_t group) {  return bgl_lock_ptr(EXT4_SB(sb)->s_blockgroup_lock, group); }
 struct ext4_buddy {  struct page *bd_buddy_page;  void *bd_buddy;  struct page *bd_bitmap_page;  void *bd_bitmap;  struct ext4_group_info *bd_info;  struct super_block *bd_sb;  __u16 bd_blkbits;  ext4_group_t bd_group; };
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) void *mb_correct_addr_and_bit(int *bit, void *addr) {  *bit += ((unsigned long) addr & 3UL) << 3;  addr = (void *) ((unsigned long) addr & ~3UL);  return addr; }
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) int mb_test_bit(int bit, void *addr) {  addr = mb_correct_addr_and_bit(&bit, addr);  return test_bit_le(bit, addr); }
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) void mb_set_bit(int bit, void *addr) {  addr = mb_correct_addr_and_bit(&bit, addr);  __set_bit_le(bit, addr); }
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) void mb_clear_bit(int bit, void *addr) {  addr = mb_correct_addr_and_bit(&bit, addr);  __clear_bit_le(bit, addr); }
 static inline __attribute__((always_inline)) __attribute__((no_instrument_function)) int mb_buddy_adjust_border(int* bit, void* bitmap, int side) {  if (mb_test_bit(*bit + side, bitmap)) {   mb_clear_bit(*bit, bitmap);   (*bit) -= side;   return 1;  }  else {   (*bit) += side;   mb_set_bit(*bit, bitmap);   return -1;  } }
 static void mb_buddy_mark_free(struct ext4_buddy *e4b, int first, int last) {  int max;  int order = 1;  void *buddy = mb_find_buddy(e4b, order, &max);  while (buddy) {   void *buddy2;   if (first & 1)    e4b->bd_info->bb_counters[order] += mb_buddy_adjust_border(&first, buddy, -1);   if (!(last & 1))    e4b->bd_info->bb_counters[order] += mb_buddy_adjust_border(&last, buddy, 1);   if (first > last)    break;   order++;   if (first == last || !(buddy2 = mb_find_buddy(e4b, order, &max))) {    mb_clear_bits(buddy, first, last - first + 1);    e4b->bd_info->bb_counters[order - 1] += last - first + 1;    break;   }   first >>= 1;   last >>= 1;   buddy = buddy2;  } }
 void mb_free_blocks(struct inode *inode, struct ext4_buddy *e4b,       int first, int count) {  int left_is_free = 0;  int right_is_free = 0;  int block;  int last = first + count - 1;  struct super_block *sb = e4b->bd_sb;  do { if (__builtin_expect(!!(last >= (sb->s_blocksize << 3)), 0)) do { printk("BUG: failure at %s:%d/%s()!\n", "fs/ext4/mballoc.c", 1400, __func__); panic("BUG!"); } while (0); } while(0);  do { if (__builtin_expect(!!(!((&(&(ext4_group_lock_ptr(sb, e4b->bd_group))->rlock)->raw_lock)->slock == 0)), 0)) do { printk("BUG: failure at %s:%d/%s()!\n", "fs/ext4/mballoc.c", 1401, __func__); panic("BUG!"); } while (0); } while(0);  ;  mb_free_blocks_double(inode, e4b, first, count);  e4b->bd_info->bb_free += count;  if (first < e4b->bd_info->bb_first_free)   e4b->bd_info->bb_first_free = first;  if (first != 0)   left_is_free = !mb_test_bit(first - 1, e4b->bd_bitmap);  block = mb_test_and_clear_bits(e4b->bd_bitmap, first, count);  if (last + 1 < EXT4_SB(sb)->s_mb_maxs[0])   right_is_free = !mb_test_bit(last + 1, e4b->bd_bitmap);  if (__builtin_expect(!!(block != -1), 0)) {   ext4_fsblk_t blocknr;   blocknr = ext4_group_first_block_no(sb, e4b->bd_group);   blocknr += ((block) << (EXT4_SB(sb))->s_cluster_bits);   __ext4_grp_locked_error(__func__,  1427   , sb, e4b->bd_group, inode ? inode->i_ino : 0, blocknr, "freeing already freed block " "(bit %u)", block)                             ;   mb_regenerate_buddy(e4b);   goto done;  }  if (left_is_free && right_is_free)   e4b->bd_info->bb_fragments--;  else if (!left_is_free && !right_is_free)   e4b->bd_info->bb_fragments++;  if (first & 1) {   first += !left_is_free;   e4b->bd_info->bb_counters[0] += left_is_free ? -1 : 1;  }  if (!(last & 1)) {   last -= !right_is_free;   e4b->bd_info->bb_counters[0] += right_is_free ? -1 : 1;  }  if (first <= last)   mb_buddy_mark_free(e4b, first >> 1, last >> 1); done:  mb_set_largest_free_order(sb, e4b->bd_info);  ; }
