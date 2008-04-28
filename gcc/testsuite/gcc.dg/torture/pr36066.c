/* { dg-do compile } */
/* { dg-options "-funsafe-loop-optimizations -ftree-vrp" } */

typedef int FLAC__int32;
typedef int FLAC__bool;
typedef struct { } FLAC__Subframe;
typedef enum { FLAC__CHANNEL_ASSIGNMENT_INDEPENDENT = 0,  FLAC__CHANNEL_ASSIGNMENT_LEFT_SIDE = 1,  FLAC__CHANNEL_ASSIGNMENT_RIGHT_SIDE = 2,  FLAC__CHANNEL_ASSIGNMENT_MID_SIDE = 3 } FLAC__ChannelAssignment;
typedef struct {
  struct FLAC__StreamEncoderProtected *protected_;
  struct FLAC__StreamEncoderPrivate *private_;
} FLAC__StreamEncoder;
typedef struct FLAC__StreamEncoderProtected {
  FLAC__bool loose_mid_side_stereo;
  unsigned channels;
  unsigned blocksize;
} FLAC__StreamEncoderProtected;
typedef struct FLAC__StreamEncoderPrivate {
  FLAC__int32 *integer_signal[(8u)];
  FLAC__Subframe subframe_workspace_mid_side[2][2];
  unsigned best_subframe_mid_side[2];
  unsigned loose_mid_side_stereo_frame_count;
} FLAC__StreamEncoderPrivate;
static void get_wasted_bits_(FLAC__int32 signal[], unsigned samples)
{
  unsigned i;
  FLAC__int32 x = 0;
  for(i = 0; i < samples && !(x&1); i++)
    x |= signal[i];
}
FLAC__Subframe * process_subframes_(FLAC__StreamEncoder *encoder, unsigned *bits)
{
  unsigned channel;
  FLAC__Subframe *left_subframe = 0;
  FLAC__ChannelAssignment channel_assignment;
  for(channel = 0; channel < encoder->protected_->channels; channel++)
      get_wasted_bits_(encoder->private_->integer_signal[channel], encoder->protected_->blocksize);
  if(encoder->protected_->loose_mid_side_stereo && encoder->private_->loose_mid_side_stereo_frame_count > 0)
      channel_assignment = FLAC__CHANNEL_ASSIGNMENT_MID_SIDE;
  else {
      FLAC__ChannelAssignment ca = (FLAC__ChannelAssignment)1;
      unsigned min_bits = bits[0];
      for(channel_assignment = (FLAC__ChannelAssignment)0; (int)ca <= 3; ca = (FLAC__ChannelAssignment)((int)ca + 1))
	  if(bits[ca] < min_bits)
	      channel_assignment = ca;
  }
  switch(channel_assignment) {
      case FLAC__CHANNEL_ASSIGNMENT_INDEPENDENT:
      case FLAC__CHANNEL_ASSIGNMENT_LEFT_SIDE:
      case FLAC__CHANNEL_ASSIGNMENT_RIGHT_SIDE:
      case FLAC__CHANNEL_ASSIGNMENT_MID_SIDE:
	  left_subframe = &encoder->private_->subframe_workspace_mid_side[0][encoder->private_->best_subframe_mid_side[0]];
  }
  return left_subframe;
}
