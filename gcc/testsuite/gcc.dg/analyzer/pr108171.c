struct nl_context {
  void *cmd_private;
};

struct sfeatures_context {
  int a;
  int req_mask[0];
};

int set_sf_req_mask_idx;

extern void fill_legacy_flag();

void
fill_sfeatures_bitmap(struct nl_context *nlctx) {
  while (nlctx) {
    fill_legacy_flag();
    struct nl_context __trans_tmp_1 = *nlctx;
    struct sfeatures_context *sfctx = __trans_tmp_1.cmd_private;
    sfctx->req_mask[set_sf_req_mask_idx] |= 1;
  }
}

void
nl_sfeatures() {
  struct nl_context nlctx;
  struct sfeatures_context *sfctx;
  nlctx.cmd_private = &sfctx;
  sfctx = 0;
  fill_sfeatures_bitmap(&nlctx);
}
