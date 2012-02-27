/* { dg-do compile } */
/* { dg-options "-O1 -mtune=amdfam10 -fexpensive-optimizations -fgcse -foptimize-register-move -freorder-blocks -fschedule-insns2 -funswitch-loops -fgcse-las -fselective-scheduling2 -fsel-sched-pipelining -funroll-all-loops" } */

typedef char uint8_t;
typedef uint32_t;
typedef vo_frame_t;
__extension__ typedef __SIZE_TYPE__ size_t;

struct vo_frame_s
{
    uint8_t base[3];
  int pitches[3];};
typedef struct
{
void
    (*proc_macro_block)
    (void);
}
xine_xvmc_t;
typedef struct
{
  uint8_t ref[2][3];
int pmv;
}
motion_t;
typedef struct
{
  uint32_t bitstream_buf;
  int bitstream_bits;
    uint8_t * bitstream_ptr;
    uint8_t dest[3];
  int pitches[3];
  int offset;
    motion_t b_motion;
    motion_t f_motion;
  int v_offset;
  int coded_picture_width;
  int picture_structure;
struct vo_frame_s *current_frame;}
picture_t;
typedef struct
{
int xvmc_last_slice_code;}
mpeg2dec_accel_t;
static int bitstream_init (picture_t * picture, void *start)
{
  picture->bitstream_ptr = start;
  return (int) (size_t) start;
}
static slice_xvmc_init (picture_t * picture, int code)
{
  int offset;
  struct vo_frame_s *forward_reference_frame;
  offset = picture->picture_structure == 2;
  picture->pitches[0] = picture->current_frame->pitches[0];
  picture->pitches[1] = picture->current_frame->pitches[1];
  if (picture)
    picture->f_motion.ref
      [0]
      [0]
      = (char) (size_t) (forward_reference_frame->base + (offset ? picture->pitches[0] : 0));
  picture->f_motion.ref[0][1] = (offset);
  if (picture->picture_structure)
      picture->pitches[0] <<= picture->pitches[1] <<= 1;
  offset = 0;
  while (1)
    {
      if (picture->bitstream_buf >= 0x08000000)
	  break;
      switch (picture->bitstream_buf >> 12)
	{
	case 8:
	  offset += 33;
		picture->bitstream_buf
		  |=
		  picture->bitstream_ptr[1] << picture->bitstream_bits;
	}
    }
  picture->offset = (offset);
  while (picture->offset - picture->coded_picture_width >= 0)
    {
      picture->offset -= picture->coded_picture_width;
      if (picture->current_frame)
	{
	  picture->dest[0] += picture->pitches[0];
	  picture->dest[1] += picture->pitches[1];
	}
      picture->v_offset += 16;
    }
}

void
mpeg2_xvmc_slice
  (mpeg2dec_accel_t * accel, picture_t * picture, int code, uint8_t buffer,int mba_inc)
{
  xine_xvmc_t * xvmc = (xine_xvmc_t *) (size_t) bitstream_init (picture, (void *) (size_t) buffer);
  slice_xvmc_init (picture, code);
    while (1)
      {
	if (picture)
	    break;
	switch (picture->bitstream_buf)
	  {
	  case 8:
	    mba_inc += accel->xvmc_last_slice_code = code;
		  xvmc->proc_macro_block   ();
	    while (mba_inc)
	      ;
	  }
      }
}
