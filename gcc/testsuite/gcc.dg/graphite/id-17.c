typedef struct
{
  int offset_for_ref_frame[256];
} seq_parameter_set_rbsp_t;

typedef struct
{
  unsigned int num_ref_frames_in_pic_order_cnt_cycle;
  int offset_for_ref_frame[1];
  int auto_crop_right;
} ImageParameters;

ImageParameters *img;
void error (const char *);

void GenerateSequenceParameterSet(seq_parameter_set_rbsp_t *sps)
{
  unsigned i;
  for (i=0; i<img->num_ref_frames_in_pic_order_cnt_cycle; i++)
    sps->offset_for_ref_frame[i] = img->offset_for_ref_frame[i];
 error("foo");
}
