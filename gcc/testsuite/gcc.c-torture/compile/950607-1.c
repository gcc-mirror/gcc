typedef struct {
  int component_id;
  int component_index;
  int h_samp_factor;
  int v_samp_factor;
} jpeg_component_info;
struct jpeg_common_struct {
  struct jpeg_error_mgr * err;
};
typedef struct jpeg_common_struct * j_common_ptr;
typedef struct jpeg_compress_struct * j_compress_ptr;
struct jpeg_compress_struct {
  struct jpeg_error_mgr * err;
  int num_components;
  jpeg_component_info * comp_info;
  int max_h_samp_factor;
  int max_v_samp_factor;
};
struct jpeg_error_mgr {
  int msg_code;
};

void
jinit_downsampler (j_compress_ptr cinfo)
{
  int ci;
  jpeg_component_info * compptr;

  for (ci = 0, compptr = cinfo->comp_info; ci < cinfo->num_components;
       ci++, compptr++) {
    if (compptr->h_samp_factor == cinfo->max_h_samp_factor &&
	compptr->v_samp_factor == cinfo->max_v_samp_factor) {
    } else if ((cinfo->max_h_samp_factor % compptr->h_samp_factor) == 0 &&
	       (cinfo->max_v_samp_factor % compptr->v_samp_factor) == 0) {
    } else
      cinfo->err->msg_code = 0;
  }
}
