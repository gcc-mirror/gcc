void kmem_free (void *);
void xfs_dir2_grow_inode(void)
{
 int map;
 int *mapp;
 int nmap;
 mapp = &map;
 if (nmap == 0 )
  mapp = ((void *)0);
 if (mapp != &map)
  kmem_free(mapp);
}
