struct foo_laptop_debug {
  struct dentry *root;
  unsigned long size;
};
struct foo_laptop {
  void *placeholder;
  struct foo_laptop_debug debug;
  char sdiag[64];
};

extern struct dentry *debugfs_create_dir(void);

void foo_debugfs_init(struct foo_laptop *foo) {
  struct dentry *root;
  root = debugfs_create_dir();
  foo->debug.root = root;
  foo->debug.size = __builtin_strlen(foo->sdiag);
}
