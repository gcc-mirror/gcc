/* { dg-do compile } */
/* { dg-options "-Os" } */

struct list_head {
 struct list_head *next;
};
void __list_del (struct list_head *);
static inline __attribute__((always_inline)) void list_del_init(struct
list_head *entry)
{
 __list_del(entry->next);
 (entry)->next = (entry);
};
struct dentry {
 void *d_fsdata;
};
struct sysfs_dirent {
 struct list_head s_sibling;
 struct list_head s_children;
};
const char *sysfs_get_name(struct sysfs_dirent *);
void sysfs_hash_and_remove(struct dentry * dir, const char * name)
{
 struct sysfs_dirent * sd;
 struct sysfs_dirent * parent_sd = dir->d_fsdata;
 for (sd = (struct sysfs_dirent *)((&parent_sd->s_children)->next);
     &sd->s_sibling != (&parent_sd->s_children);
     sd  = (struct sysfs_dirent *)sd->s_sibling.next) {
  if (!__builtin_strcmp(sysfs_get_name(sd), name))
  {
   list_del_init(&sd->s_sibling);
   break;
  }
 }
}
