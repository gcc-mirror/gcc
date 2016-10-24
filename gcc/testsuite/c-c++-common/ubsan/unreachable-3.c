/* { dg-do compile } */
/* { dg-options "-fsanitize=unreachable -O2 -fno-reorder-blocks -fsanitize-coverage=trace-pc -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

extern unsigned int ioread32(void *);
struct vnic_wq_ctrl {
    unsigned int error_status;
};
struct snic {
    unsigned int wq_count;
    struct vnic_wq_ctrl *wq[1];
    int wq_lock[1];
};
void snic_log_q_error(struct snic *snic)
{
    unsigned int i;
    for (i = 0; i < snic->wq_count; i++)
        ioread32(&snic->wq[i]->error_status);
}

/* { dg-final { scan-tree-dump "__builtin___ubsan_handle_builtin_unreachable" "optimized" } } */
