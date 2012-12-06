/* { dg-do compile } */
/* { dg-options "-funswitch-loops -ftree-loop-distribution" } */

typedef struct AVProgram {
    void *priv_data;
    unsigned int nb_streams;
} AVFormatContext;
typedef struct {
    unsigned short flags;
    unsigned char stream_id;
} FrameCode;
typedef struct {
    FrameCode frame_code[256];
} NUTContext;
void build_frame_code(AVFormatContext *s, int stream_id,
		      int is_audio, int pred_count)
{
  NUTContext *nut = s->priv_data;
  int keyframe_0_esc = s->nb_streams > 2;
  int start2 = 1 + 253*stream_id / s->nb_streams;
  int key_frame;
  for(key_frame=0; key_frame<2; key_frame++)
    {
      FrameCode *ft;
      if (is_audio && keyframe_0_esc && key_frame==0)
	continue;
      ft= &nut->frame_code[start2];
      ft->flags|= 3;
      ft->stream_id= stream_id;
    }
}
