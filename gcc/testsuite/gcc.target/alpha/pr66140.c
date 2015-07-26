/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ev4" } */

struct scsi_cmnd {
	int sc_data_direction;
};
struct lpfc_hba {
	unsigned cfg_total_seg_cnt;
};
struct lpfc_scsi_buf {
	struct scsi_cmnd *pCmd;
	unsigned seg_cnt;
	unsigned *fcp_bpl;
};

extern void *sg_next(void *sg);
extern void *scsi_sglist(struct scsi_cmnd *cmd);
extern unsigned scsi_sg_count(struct scsi_cmnd *cmd);

static inline void dma_map_sg_attrs(void *sg, int nents, int dir)
{
	int i;

	for (i = 0; i < nents; i++, sg = sg_next(sg))
		;

	if (!dir)
		asm volatile( "call_pal %0" : : "i"(129));
}

static inline void lpfc_bg_setup_bpl(struct lpfc_hba *phba, struct scsi_cmnd *sc,
				     unsigned *pde5)
{
	void *sgde;
	int i;

	*pde5 = (((0x85 & 0x000000ffu) << 24) | (*pde5 & ~(0x000000ffu << 24)));
	for (i = 0, sgde = scsi_sglist(sc); i < 2; i++, sgde = sg_next(sgde))
		;
}

void lpfc_bg_scsi_prep_dma_buf_s3(struct lpfc_hba *phba,
				  struct lpfc_scsi_buf *lpfc_cmd)
{
	struct scsi_cmnd *scsi_cmnd = lpfc_cmd->pCmd;
	unsigned *bpl = lpfc_cmd->fcp_bpl;

	dma_map_sg_attrs(scsi_sglist(scsi_cmnd),
			 scsi_sg_count(scsi_cmnd),
			 scsi_cmnd->sc_data_direction);
	if (lpfc_cmd->seg_cnt > phba->cfg_total_seg_cnt)
		lpfc_bg_setup_bpl(phba, scsi_cmnd, bpl);
}
