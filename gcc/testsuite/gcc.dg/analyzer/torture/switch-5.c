/* { dg-additional-options "-fno-analyzer-call-summaries" } */

typedef unsigned char u8;
typedef signed int s32;
typedef unsigned int u32;

enum v4l2_mpeg_video_hevc_profile {
  V4L2_MPEG_VIDEO_HEVC_PROFILE_MAIN = 0,
  V4L2_MPEG_VIDEO_HEVC_PROFILE_MAIN_STILL_PICTURE = 1,
  V4L2_MPEG_VIDEO_HEVC_PROFILE_MAIN_10 = 2
};
enum v4l2_buf_type {
  V4L2_BUF_TYPE_VIDEO_CAPTURE = 1,
  V4L2_BUF_TYPE_VIDEO_OUTPUT = 2
};
struct v4l2_fmtdesc {
  u32 index;
  u32 type;
};
struct v4l2_ctrl;
s32 v4l2_ctrl_g_ctrl(struct v4l2_ctrl *ctrl);
struct create_channel_param {
  u8 profile;
};

u8
hevc_profile_to_mcu_profile(enum v4l2_mpeg_video_hevc_profile profile) {
  switch (profile) {
  default:
  case V4L2_MPEG_VIDEO_HEVC_PROFILE_MAIN:
    return 1;
  case V4L2_MPEG_VIDEO_HEVC_PROFILE_MAIN_10:
    return 2;
  case V4L2_MPEG_VIDEO_HEVC_PROFILE_MAIN_STILL_PICTURE:
    return 3;
  }
}

int fill_create_channel_param(struct v4l2_ctrl *ctrl,
			      struct create_channel_param *param) {
  enum v4l2_mpeg_video_hevc_profile profile;
  profile = v4l2_ctrl_g_ctrl(ctrl);
  param->profile = hevc_profile_to_mcu_profile(profile);
  return 0;
}

int allegro_enum_fmt_vid(struct v4l2_fmtdesc *f) {
  switch (f->type) {
  case V4L2_BUF_TYPE_VIDEO_OUTPUT:
    if (f->index >= 1)
      return -22;
    break;
  case V4L2_BUF_TYPE_VIDEO_CAPTURE:
    if (f->index >= 2)
      return -22;
    break;
  default:
    return -22;
  }
  return 0;
}

int allegro_ioctl_streamon(struct v4l2_ctrl *ctrl,
			   struct create_channel_param *param) {
  fill_create_channel_param(ctrl, param);

  return 0;
}
