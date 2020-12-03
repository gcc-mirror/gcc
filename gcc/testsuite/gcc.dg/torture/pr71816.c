/* { dg-do compile } */
/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch" } */

void *ext2fs_resize_mem_p;
struct ext2_icount_el {
    int ino;
} * insert_icount_el_icount_1;
int insert_icount_el_icount, insert_icount_el_new_size;
void *memcpy();
void *realloc();
int ext2fs_resize_mem(void *p1) {
    int size = 0;
    memcpy(&ext2fs_resize_mem_p, p1, sizeof(ext2fs_resize_mem_p));
    void *p = realloc(&ext2fs_resize_mem_p, size);
    return 0;
}
struct ext2_icount_el *insert_icount_el() {
    if (insert_icount_el_icount)
      insert_icount_el_new_size = insert_icount_el_icount_1[0].ino;
    ext2fs_resize_mem(&insert_icount_el_icount_1);
    return 0;
}

/* Passing the address of a declared object to realloc triggers
   -Wfree-nonheap-object unless -flto is used.
   { dg-prune-output "\\\[-Wfree-nonheap-object" } */
