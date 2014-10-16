/* { dg-do run } */
/* { dg-shouldfail "asan" } */

#include <stdbool.h>
#include <sanitizer/asan_interface.h>

struct vfsmount {};
struct dentry {};

struct path {
  struct vfsmount *mnt;
  struct dentry *dentry;
};

struct fs_struct {
  int users;
  int lock;
  int seq;
  int umask;
  int in_exec;
  struct path root, pwd;
};

void __attribute__((noinline, noclone))
copy_fs_struct(struct fs_struct *a, struct fs_struct *b) {
  a->root = b->root;
}

struct fs_struct a, b;

int
main () {
  __asan_poison_memory_region (&a.root, sizeof (a.root));
  copy_fs_struct (&a, &b);
  return 0;
}

/* { dg-output "ERROR: AddressSanitizer:\[^\n\r]*on address\[^\n\r]*" } */
