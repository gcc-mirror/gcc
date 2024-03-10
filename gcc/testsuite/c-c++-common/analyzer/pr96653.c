/* Examples of switch statements with many cases (with default values).
   Adapted from Linux 5.9-rc1:drivers/media/v4l2-core/v4l2-ctrls.c.  */

/* { dg-additional-options "-O1" } */

typedef unsigned int u32;
typedef long long s64;
typedef unsigned long long u64;

enum v4l2_ctrl_type {
  V4L2_CTRL_TYPE_INTEGER = 1,
  V4L2_CTRL_TYPE_BOOLEAN = 2,
  V4L2_CTRL_TYPE_MENU = 3,
  V4L2_CTRL_TYPE_BUTTON = 4,
  V4L2_CTRL_TYPE_INTEGER64 = 5,
  V4L2_CTRL_TYPE_CTRL_CLASS = 6,
  V4L2_CTRL_TYPE_STRING = 7,
  V4L2_CTRL_TYPE_BITMASK = 8,
  V4L2_CTRL_TYPE_INTEGER_MENU = 9,

  V4L2_CTRL_COMPOUND_TYPES = 0x0100,
  V4L2_CTRL_TYPE_U8 = 0x0100,
  V4L2_CTRL_TYPE_U16 = 0x0101,
  V4L2_CTRL_TYPE_U32 = 0x0102,
  V4L2_CTRL_TYPE_AREA = 0x0106,
};

const char *v4l2_ctrl_get_name(u32 id) {
  switch (id) {
  case (0x00980000 | 1):
    return "User Controls";
  case ((0x00980000 | 0x900) + 0):
    return "Brightness";
  case ((0x00980000 | 0x900) + 1):
    return "Contrast";
  case ((0x00980000 | 0x900) + 2):
    return "Saturation";
  case ((0x00980000 | 0x900) + 3):
    return "Hue";
  case ((0x00980000 | 0x900) + 5):
    return "Volume";
  case ((0x00980000 | 0x900) + 6):
    return "Balance";
  case ((0x00980000 | 0x900) + 7):
    return "Bass";
  case ((0x00980000 | 0x900) + 8):
    return "Treble";
  case ((0x00980000 | 0x900) + 9):
    return "Mute";
  case ((0x00980000 | 0x900) + 10):
    return "Loudness";
  case ((0x00980000 | 0x900) + 11):
    return "Black Level";
  case ((0x00980000 | 0x900) + 12):
    return "White Balance, Automatic";
  case ((0x00980000 | 0x900) + 13):
    return "Do White Balance";
  case ((0x00980000 | 0x900) + 14):
    return "Red Balance";
  case ((0x00980000 | 0x900) + 15):
    return "Blue Balance";
  case ((0x00980000 | 0x900) + 16):
    return "Gamma";
  case ((0x00980000 | 0x900) + 17):
    return "Exposure";
  case ((0x00980000 | 0x900) + 18):
    return "Gain, Automatic";
  case ((0x00980000 | 0x900) + 19):
    return "Gain";
  case ((0x00980000 | 0x900) + 20):
    return "Horizontal Flip";
  case ((0x00980000 | 0x900) + 21):
    return "Vertical Flip";
  case ((0x00980000 | 0x900) + 24):
    return "Power Line Frequency";
  case ((0x00980000 | 0x900) + 25):
    return "Hue, Automatic";
  case ((0x00980000 | 0x900) + 26):
    return "White Balance Temperature";
  case ((0x00980000 | 0x900) + 27):
    return "Sharpness";
  case ((0x00980000 | 0x900) + 28):
    return "Backlight Compensation";
  case ((0x00980000 | 0x900) + 29):
    return "Chroma AGC";
  case ((0x00980000 | 0x900) + 30):
    return "Color Killer";
  case ((0x00980000 | 0x900) + 31):
    return "Color Effects";
  case ((0x00980000 | 0x900) + 32):
    return "Brightness, Automatic";
  case ((0x00980000 | 0x900) + 33):
    return "Band-Stop Filter";
  case ((0x00980000 | 0x900) + 34):
    return "Rotate";
  case ((0x00980000 | 0x900) + 35):
    return "Background Color";
  case ((0x00980000 | 0x900) + 36):
    return "Chroma Gain";
  case ((0x00980000 | 0x900) + 37):
    return "Illuminator 1";
  case ((0x00980000 | 0x900) + 38):
    return "Illuminator 2";
  case ((0x00980000 | 0x900) + 39):
    return "Min Number of Capture Buffers";
  case ((0x00980000 | 0x900) + 40):
    return "Min Number of Output Buffers";
  case ((0x00980000 | 0x900) + 41):
    return "Alpha Component";
  case ((0x00980000 | 0x900) + 42):
    return "Color Effects, CbCr";
  case (0x00990000 | 1):
    return "Codec Controls";
  case ((0x00990000 | 0x900) + 0):
    return "Stream Type";
  case ((0x00990000 | 0x900) + 1):
    return "Stream PMT Program ID";
  case ((0x00990000 | 0x900) + 2):
    return "Stream Audio Program ID";
  case ((0x00990000 | 0x900) + 3):
    return "Stream Video Program ID";
  case ((0x00990000 | 0x900) + 4):
    return "Stream PCR Program ID";
  case ((0x00990000 | 0x900) + 5):
    return "Stream PES Audio ID";
  case ((0x00990000 | 0x900) + 6):
    return "Stream PES Video ID";
  case ((0x00990000 | 0x900) + 7):
    return "Stream VBI Format";
  case ((0x00990000 | 0x900) + 100):
    return "Audio Sampling Frequency";
  case ((0x00990000 | 0x900) + 101):
    return "Audio Encoding";
  case ((0x00990000 | 0x900) + 102):
    return "Audio Layer I Bitrate";
  case ((0x00990000 | 0x900) + 103):
    return "Audio Layer II Bitrate";
  case ((0x00990000 | 0x900) + 104):
    return "Audio Layer III Bitrate";
  case ((0x00990000 | 0x900) + 105):
    return "Audio Stereo Mode";
  case ((0x00990000 | 0x900) + 106):
    return "Audio Stereo Mode Extension";
  case ((0x00990000 | 0x900) + 107):
    return "Audio Emphasis";
  case ((0x00990000 | 0x900) + 108):
    return "Audio CRC";
  case ((0x00990000 | 0x900) + 109):
    return "Audio Mute";
  case ((0x00990000 | 0x900) + 110):
    return "Audio AAC Bitrate";
  case ((0x00990000 | 0x900) + 111):
    return "Audio AC-3 Bitrate";
  case ((0x00990000 | 0x900) + 112):
    return "Audio Playback";
  case ((0x00990000 | 0x900) + 113):
    return "Audio Multilingual Playback";
  case ((0x00990000 | 0x900) + 200):
    return "Video Encoding";
  case ((0x00990000 | 0x900) + 201):
    return "Video Aspect";
  case ((0x00990000 | 0x900) + 202):
    return "Video B Frames";
  case ((0x00990000 | 0x900) + 203):
    return "Video GOP Size";
  case ((0x00990000 | 0x900) + 204):
    return "Video GOP Closure";
  case ((0x00990000 | 0x900) + 205):
    return "Video Pulldown";
  case ((0x00990000 | 0x900) + 206):
    return "Video Bitrate Mode";
  case ((0x00990000 | 0x900) + 207):
    return "Video Bitrate";
  case ((0x00990000 | 0x900) + 208):
    return "Video Peak Bitrate";
  case ((0x00990000 | 0x900) + 209):
    return "Video Temporal Decimation";
  case ((0x00990000 | 0x900) + 210):
    return "Video Mute";
  case ((0x00990000 | 0x900) + 211):
    return "Video Mute YUV";
  case ((0x00990000 | 0x900) + 212):
    return "Decoder Slice Interface";
  case ((0x00990000 | 0x900) + 213):
    return "MPEG4 Loop Filter Enable";
  case ((0x00990000 | 0x900) + 214):
    return "Number of Intra Refresh MBs";
  case ((0x00990000 | 0x900) + 215):
    return "Frame Level Rate Control Enable";
  case ((0x00990000 | 0x900) + 218):
    return "H264 MB Level Rate Control";
  case ((0x00990000 | 0x900) + 216):
    return "Sequence Header Mode";
  case ((0x00990000 | 0x900) + 217):
    return "Max Number of Reference Pics";
  case ((0x00990000 | 0x900) + 300):
    return "H263 I-Frame QP Value";
  case ((0x00990000 | 0x900) + 301):
    return "H263 P-Frame QP Value";
  case ((0x00990000 | 0x900) + 302):
    return "H263 B-Frame QP Value";
  case ((0x00990000 | 0x900) + 303):
    return "H263 Minimum QP Value";
  case ((0x00990000 | 0x900) + 304):
    return "H263 Maximum QP Value";
  case ((0x00990000 | 0x900) + 350):
    return "H264 I-Frame QP Value";
  case ((0x00990000 | 0x900) + 351):
    return "H264 P-Frame QP Value";
  case ((0x00990000 | 0x900) + 352):
    return "H264 B-Frame QP Value";
  case ((0x00990000 | 0x900) + 354):
    return "H264 Maximum QP Value";
  case ((0x00990000 | 0x900) + 353):
    return "H264 Minimum QP Value";
  case ((0x00990000 | 0x900) + 355):
    return "H264 8x8 Transform Enable";
  case ((0x00990000 | 0x900) + 356):
    return "H264 CPB Buffer Size";
  case ((0x00990000 | 0x900) + 357):
    return "H264 Entropy Mode";
  case ((0x00990000 | 0x900) + 358):
    return "H264 I-Frame Period";
  case ((0x00990000 | 0x900) + 359):
    return "H264 Level";
  case ((0x00990000 | 0x900) + 360):
    return "H264 Loop Filter Alpha Offset";
  case ((0x00990000 | 0x900) + 361):
    return "H264 Loop Filter Beta Offset";
  case ((0x00990000 | 0x900) + 362):
    return "H264 Loop Filter Mode";
  case ((0x00990000 | 0x900) + 363):
    return "H264 Profile";
  case ((0x00990000 | 0x900) + 364):
    return "Vertical Size of SAR";
  case ((0x00990000 | 0x900) + 365):
    return "Horizontal Size of SAR";
  case ((0x00990000 | 0x900) + 366):
    return "Aspect Ratio VUI Enable";
  case ((0x00990000 | 0x900) + 367):
    return "VUI Aspect Ratio IDC";
  case ((0x00990000 | 0x900) + 368):
    return "H264 Enable Frame Packing SEI";
  case ((0x00990000 | 0x900) + 369):
    return "H264 Set Curr. Frame as Frame0";
  case ((0x00990000 | 0x900) + 370):
    return "H264 FP Arrangement Type";
  case ((0x00990000 | 0x900) + 371):
    return "H264 Flexible MB Ordering";
  case ((0x00990000 | 0x900) + 372):
    return "H264 Map Type for FMO";
  case ((0x00990000 | 0x900) + 373):
    return "H264 FMO Number of Slice Groups";
  case ((0x00990000 | 0x900) + 374):
    return "H264 FMO Direction of Change";
  case ((0x00990000 | 0x900) + 375):
    return "H264 FMO Size of 1st Slice Grp";
  case ((0x00990000 | 0x900) + 376):
    return "H264 FMO No. of Consecutive MBs";
  case ((0x00990000 | 0x900) + 377):
    return "H264 Arbitrary Slice Ordering";
  case ((0x00990000 | 0x900) + 378):
    return "H264 ASO Slice Order";
  case ((0x00990000 | 0x900) + 379):
    return "Enable H264 Hierarchical Coding";
  case ((0x00990000 | 0x900) + 380):
    return "H264 Hierarchical Coding Type";
  case ((0x00990000 | 0x900) + 381):
    return "H264 Number of HC Layers";
  case ((0x00990000 | 0x900) + 382):
    return "H264 Set QP Value for HC Layers";
  case ((0x00990000 | 0x900) + 383):
    return "H264 Constrained Intra Pred";
  case ((0x00990000 | 0x900) + 384):
    return "H264 Chroma QP Index Offset";
  case ((0x00990000 | 0x900) + 385):
    return "H264 I-Frame Minimum QP Value";
  case ((0x00990000 | 0x900) + 386):
    return "H264 I-Frame Maximum QP Value";
  case ((0x00990000 | 0x900) + 387):
    return "H264 P-Frame Minimum QP Value";
  case ((0x00990000 | 0x900) + 388):
    return "H264 P-Frame Maximum QP Value";
  case ((0x00990000 | 0x900) + 1000):
    return "H264 Sequence Parameter Set";
  case ((0x00990000 | 0x900) + 1001):
    return "H264 Picture Parameter Set";
  case ((0x00990000 | 0x900) + 1002):
    return "H264 Scaling Matrix";
  case ((0x00990000 | 0x900) + 1003):
    return "H264 Slice Parameters";
  case ((0x00990000 | 0x900) + 1004):
    return "H264 Decode Parameters";
  case ((0x00990000 | 0x900) + 1005):
    return "H264 Decode Mode";
  case ((0x00990000 | 0x900) + 1006):
    return "H264 Start Code";
  case ((0x00990000 | 0x900) + 270):
    return "MPEG2 Level";
  case ((0x00990000 | 0x900) + 271):
    return "MPEG2 Profile";
  case ((0x00990000 | 0x900) + 400):
    return "MPEG4 I-Frame QP Value";
  case ((0x00990000 | 0x900) + 401):
    return "MPEG4 P-Frame QP Value";
  case ((0x00990000 | 0x900) + 402):
    return "MPEG4 B-Frame QP Value";
  case ((0x00990000 | 0x900) + 403):
    return "MPEG4 Minimum QP Value";
  case ((0x00990000 | 0x900) + 404):
    return "MPEG4 Maximum QP Value";
  case ((0x00990000 | 0x900) + 405):
    return "MPEG4 Level";
  case ((0x00990000 | 0x900) + 406):
    return "MPEG4 Profile";
  case ((0x00990000 | 0x900) + 407):
    return "Quarter Pixel Search Enable";
  case ((0x00990000 | 0x900) + 219):
    return "Maximum Bytes in a Slice";
  case ((0x00990000 | 0x900) + 220):
    return "Number of MBs in a Slice";
  case ((0x00990000 | 0x900) + 221):
    return "Slice Partitioning Method";
  case ((0x00990000 | 0x900) + 222):
    return "VBV Buffer Size";
  case ((0x00990000 | 0x900) + 223):
    return "Video Decoder PTS";
  case ((0x00990000 | 0x900) + 224):
    return "Video Decoder Frame Count";
  case ((0x00990000 | 0x900) + 225):
    return "Initial Delay for VBV Control";
  case ((0x00990000 | 0x900) + 227):
    return "Horizontal MV Search Range";
  case ((0x00990000 | 0x900) + 228):
    return "Vertical MV Search Range";
  case ((0x00990000 | 0x900) + 226):
    return "Repeat Sequence Header";
  case ((0x00990000 | 0x900) + 229):
    return "Force Key Frame";
  case ((0x00990000 | 0x900) + 250):
    return "MPEG-2 Slice Parameters";
  case ((0x00990000 | 0x900) + 251):
    return "MPEG-2 Quantization Matrices";
  case ((0x00990000 | 0x900) + 292):
    return "FWHT Stateless Parameters";
  case ((0x00990000 | 0x900) + 290):
    return "FWHT I-Frame QP Value";
  case ((0x00990000 | 0x900) + 291):
    return "FWHT P-Frame QP Value";

  case ((0x00990000 | 0x900) + 500):
    return "VPX Number of Partitions";
  case ((0x00990000 | 0x900) + 501):
    return "VPX Intra Mode Decision Disable";
  case ((0x00990000 | 0x900) + 502):
    return "VPX No. of Refs for P Frame";
  case ((0x00990000 | 0x900) + 503):
    return "VPX Loop Filter Level Range";
  case ((0x00990000 | 0x900) + 504):
    return "VPX Deblocking Effect Control";
  case ((0x00990000 | 0x900) + 505):
    return "VPX Golden Frame Refresh Period";
  case ((0x00990000 | 0x900) + 506):
    return "VPX Golden Frame Indicator";
  case ((0x00990000 | 0x900) + 507):
    return "VPX Minimum QP Value";
  case ((0x00990000 | 0x900) + 508):
    return "VPX Maximum QP Value";
  case ((0x00990000 | 0x900) + 509):
    return "VPX I-Frame QP Value";
  case ((0x00990000 | 0x900) + 510):
    return "VPX P-Frame QP Value";
  case ((0x00990000 | 0x900) + 511):
    return "VP8 Profile";
  case ((0x00990000 | 0x900) + 512):
    return "VP9 Profile";
  case ((0x00990000 | 0x900) + 2000):
    return "VP8 Frame Header";

  case ((0x00990000 | 0x900) + 602):
    return "HEVC I-Frame QP Value";
  case ((0x00990000 | 0x900) + 603):
    return "HEVC P-Frame QP Value";
  case ((0x00990000 | 0x900) + 604):
    return "HEVC B-Frame QP Value";
  case ((0x00990000 | 0x900) + 600):
    return "HEVC Minimum QP Value";
  case ((0x00990000 | 0x900) + 601):
    return "HEVC Maximum QP Value";
  case ((0x00990000 | 0x900) + 615):
    return "HEVC Profile";
  case ((0x00990000 | 0x900) + 616):
    return "HEVC Level";
  case ((0x00990000 | 0x900) + 618):
    return "HEVC Tier";
  case ((0x00990000 | 0x900) + 617):
    return "HEVC Frame Rate Resolution";
  case ((0x00990000 | 0x900) + 619):
    return "HEVC Maximum Coding Unit Depth";
  case ((0x00990000 | 0x900) + 623):
    return "HEVC Refresh Type";
  case ((0x00990000 | 0x900) + 626):
    return "HEVC Constant Intra Prediction";
  case ((0x00990000 | 0x900) + 625):
    return "HEVC Lossless Encoding";
  case ((0x00990000 | 0x900) + 627):
    return "HEVC Wavefront";
  case ((0x00990000 | 0x900) + 620):
    return "HEVC Loop Filter";
  case ((0x00990000 | 0x900) + 605):
    return "HEVC QP Values";
  case ((0x00990000 | 0x900) + 606):
    return "HEVC Hierarchical Coding Type";
  case ((0x00990000 | 0x900) + 607):
    return "HEVC Hierarchical Coding Layer";
  case ((0x00990000 | 0x900) + 608):
    return "HEVC Hierarchical Layer 0 QP";
  case ((0x00990000 | 0x900) + 609):
    return "HEVC Hierarchical Layer 1 QP";
  case ((0x00990000 | 0x900) + 610):
    return "HEVC Hierarchical Layer 2 QP";
  case ((0x00990000 | 0x900) + 611):
    return "HEVC Hierarchical Layer 3 QP";
  case ((0x00990000 | 0x900) + 612):
    return "HEVC Hierarchical Layer 4 QP";
  case ((0x00990000 | 0x900) + 613):
    return "HEVC Hierarchical Layer 5 QP";
  case ((0x00990000 | 0x900) + 614):
    return "HEVC Hierarchical Layer 6 QP";
  case ((0x00990000 | 0x900) + 636):
    return "HEVC Hierarchical Lay 0 BitRate";
  case ((0x00990000 | 0x900) + 637):
    return "HEVC Hierarchical Lay 1 BitRate";
  case ((0x00990000 | 0x900) + 638):
    return "HEVC Hierarchical Lay 2 BitRate";
  case ((0x00990000 | 0x900) + 639):
    return "HEVC Hierarchical Lay 3 BitRate";
  case ((0x00990000 | 0x900) + 640):
    return "HEVC Hierarchical Lay 4 BitRate";
  case ((0x00990000 | 0x900) + 641):
    return "HEVC Hierarchical Lay 5 BitRate";
  case ((0x00990000 | 0x900) + 642):
    return "HEVC Hierarchical Lay 6 BitRate";
  case ((0x00990000 | 0x900) + 628):
    return "HEVC General PB";
  case ((0x00990000 | 0x900) + 629):
    return "HEVC Temporal ID";
  case ((0x00990000 | 0x900) + 630):
    return "HEVC Strong Intra Smoothing";
  case ((0x00990000 | 0x900) + 632):
    return "HEVC Intra PU Split";
  case ((0x00990000 | 0x900) + 633):
    return "HEVC TMV Prediction";
  case ((0x00990000 | 0x900) + 631):
    return "HEVC Max Num of Candidate MVs";
  case ((0x00990000 | 0x900) + 634):
    return "HEVC ENC Without Startcode";
  case ((0x00990000 | 0x900) + 624):
    return "HEVC Num of I-Frame b/w 2 IDR";
  case ((0x00990000 | 0x900) + 621):
    return "HEVC Loop Filter Beta Offset";
  case ((0x00990000 | 0x900) + 622):
    return "HEVC Loop Filter TC Offset";
  case ((0x00990000 | 0x900) + 635):
    return "HEVC Size of Length Field";
  case ((0x00990000 | 0x900) + 643):
    return "Reference Frames for a P-Frame";
  case ((0x00990000 | 0x900) + 644):
    return "Prepend SPS and PPS to IDR";
  case ((0x00990000 | 0x900) + 1008):
    return "HEVC Sequence Parameter Set";
  case ((0x00990000 | 0x900) + 1009):
    return "HEVC Picture Parameter Set";
  case ((0x00990000 | 0x900) + 1010):
    return "HEVC Slice Parameters";
  case ((0x00990000 | 0x900) + 1015):
    return "HEVC Decode Mode";
  case ((0x00990000 | 0x900) + 1016):
    return "HEVC Start Code";

  case (0x009a0000 | 1):
    return "Camera Controls";
  case ((0x009a0000 | 0x900) + 1):
    return "Auto Exposure";
  case ((0x009a0000 | 0x900) + 2):
    return "Exposure Time, Absolute";
  case ((0x009a0000 | 0x900) + 3):
    return "Exposure, Dynamic Framerate";
  case ((0x009a0000 | 0x900) + 4):
    return "Pan, Relative";
  case ((0x009a0000 | 0x900) + 5):
    return "Tilt, Relative";
  case ((0x009a0000 | 0x900) + 6):
    return "Pan, Reset";
  case ((0x009a0000 | 0x900) + 7):
    return "Tilt, Reset";
  case ((0x009a0000 | 0x900) + 8):
    return "Pan, Absolute";
  case ((0x009a0000 | 0x900) + 9):
    return "Tilt, Absolute";
  case ((0x009a0000 | 0x900) + 10):
    return "Focus, Absolute";
  case ((0x009a0000 | 0x900) + 11):
    return "Focus, Relative";
  case ((0x009a0000 | 0x900) + 12):
    return "Focus, Automatic Continuous";
  case ((0x009a0000 | 0x900) + 13):
    return "Zoom, Absolute";
  case ((0x009a0000 | 0x900) + 14):
    return "Zoom, Relative";
  case ((0x009a0000 | 0x900) + 15):
    return "Zoom, Continuous";
  case ((0x009a0000 | 0x900) + 16):
    return "Privacy";
  case ((0x009a0000 | 0x900) + 17):
    return "Iris, Absolute";
  case ((0x009a0000 | 0x900) + 18):
    return "Iris, Relative";
  case ((0x009a0000 | 0x900) + 19):
    return "Auto Exposure, Bias";
  case ((0x009a0000 | 0x900) + 20):
    return "White Balance, Auto & Preset";
  case ((0x009a0000 | 0x900) + 21):
    return "Wide Dynamic Range";
  case ((0x009a0000 | 0x900) + 22):
    return "Image Stabilization";
  case ((0x009a0000 | 0x900) + 23):
    return "ISO Sensitivity";
  case ((0x009a0000 | 0x900) + 24):
    return "ISO Sensitivity, Auto";
  case ((0x009a0000 | 0x900) + 25):
    return "Exposure, Metering Mode";
  case ((0x009a0000 | 0x900) + 26):
    return "Scene Mode";
  case ((0x009a0000 | 0x900) + 27):
    return "3A Lock";
  case ((0x009a0000 | 0x900) + 28):
    return "Auto Focus, Start";
  case ((0x009a0000 | 0x900) + 29):
    return "Auto Focus, Stop";
  case ((0x009a0000 | 0x900) + 30):
    return "Auto Focus, Status";
  case ((0x009a0000 | 0x900) + 31):
    return "Auto Focus, Range";
  case ((0x009a0000 | 0x900) + 32):
    return "Pan, Speed";
  case ((0x009a0000 | 0x900) + 33):
    return "Tilt, Speed";
  case ((0x009e0000 | 0x900) + 8):
    return "Unit Cell Size";
  case ((0x009a0000 | 0x900) + 34):
    return "Camera Orientation";
  case ((0x009a0000 | 0x900) + 35):
    return "Camera Sensor Rotation";

  case (0x009b0000 | 1):
    return "FM Radio Modulator Controls";
  case ((0x009b0000 | 0x900) + 1):
    return "RDS Signal Deviation";
  case ((0x009b0000 | 0x900) + 2):
    return "RDS Program ID";
  case ((0x009b0000 | 0x900) + 3):
    return "RDS Program Type";
  case ((0x009b0000 | 0x900) + 5):
    return "RDS PS Name";
  case ((0x009b0000 | 0x900) + 6):
    return "RDS Radio Text";
  case ((0x009b0000 | 0x900) + 7):
    return "RDS Stereo";
  case ((0x009b0000 | 0x900) + 8):
    return "RDS Artificial Head";
  case ((0x009b0000 | 0x900) + 9):
    return "RDS Compressed";
  case ((0x009b0000 | 0x900) + 10):
    return "RDS Dynamic PTY";
  case ((0x009b0000 | 0x900) + 11):
    return "RDS Traffic Announcement";
  case ((0x009b0000 | 0x900) + 12):
    return "RDS Traffic Program";
  case ((0x009b0000 | 0x900) + 13):
    return "RDS Music";
  case ((0x009b0000 | 0x900) + 14):
    return "RDS Enable Alt Frequencies";
  case ((0x009b0000 | 0x900) + 15):
    return "RDS Alternate Frequencies";
  case ((0x009b0000 | 0x900) + 64):
    return "Audio Limiter Feature Enabled";
  case ((0x009b0000 | 0x900) + 65):
    return "Audio Limiter Release Time";
  case ((0x009b0000 | 0x900) + 66):
    return "Audio Limiter Deviation";
  case ((0x009b0000 | 0x900) + 80):
    return "Audio Compression Enabled";
  case ((0x009b0000 | 0x900) + 81):
    return "Audio Compression Gain";
  case ((0x009b0000 | 0x900) + 82):
    return "Audio Compression Threshold";
  case ((0x009b0000 | 0x900) + 83):
    return "Audio Compression Attack Time";
  case ((0x009b0000 | 0x900) + 84):
    return "Audio Compression Release Time";
  case ((0x009b0000 | 0x900) + 96):
    return "Pilot Tone Feature Enabled";
  case ((0x009b0000 | 0x900) + 97):
    return "Pilot Tone Deviation";
  case ((0x009b0000 | 0x900) + 98):
    return "Pilot Tone Frequency";
  case ((0x009b0000 | 0x900) + 112):
    return "Pre-Emphasis";
  case ((0x009b0000 | 0x900) + 113):
    return "Tune Power Level";
  case ((0x009b0000 | 0x900) + 114):
    return "Tune Antenna Capacitor";

  case (0x009c0000 | 1):
    return "Flash Controls";
  case ((0x009c0000 | 0x900) + 1):
    return "LED Mode";
  case ((0x009c0000 | 0x900) + 2):
    return "Strobe Source";
  case ((0x009c0000 | 0x900) + 3):
    return "Strobe";
  case ((0x009c0000 | 0x900) + 4):
    return "Stop Strobe";
  case ((0x009c0000 | 0x900) + 5):
    return "Strobe Status";
  case ((0x009c0000 | 0x900) + 6):
    return "Strobe Timeout";
  case ((0x009c0000 | 0x900) + 7):
    return "Intensity, Flash Mode";
  case ((0x009c0000 | 0x900) + 8):
    return "Intensity, Torch Mode";
  case ((0x009c0000 | 0x900) + 9):
    return "Intensity, Indicator";
  case ((0x009c0000 | 0x900) + 10):
    return "Faults";
  case ((0x009c0000 | 0x900) + 11):
    return "Charge";
  case ((0x009c0000 | 0x900) + 12):
    return "Ready to Strobe";

  case (0x009d0000 | 1):
    return "JPEG Compression Controls";
  case ((0x009d0000 | 0x900) + 1):
    return "Chroma Subsampling";
  case ((0x009d0000 | 0x900) + 2):
    return "Restart Interval";
  case ((0x009d0000 | 0x900) + 3):
    return "Compression Quality";
  case ((0x009d0000 | 0x900) + 4):
    return "Active Markers";

  case (0x009e0000 | 1):
    return "Image Source Controls";
  case ((0x009e0000 | 0x900) + 1):
    return "Vertical Blanking";
  case ((0x009e0000 | 0x900) + 2):
    return "Horizontal Blanking";
  case ((0x009e0000 | 0x900) + 3):
    return "Analogue Gain";
  case ((0x009e0000 | 0x900) + 4):
    return "Red Pixel Value";
  case ((0x009e0000 | 0x900) + 5):
    return "Green (Red) Pixel Value";
  case ((0x009e0000 | 0x900) + 6):
    return "Blue Pixel Value";
  case ((0x009e0000 | 0x900) + 7):
    return "Green (Blue) Pixel Value";

  case (0x009f0000 | 1):
    return "Image Processing Controls";
  case ((0x009f0000 | 0x900) + 1):
    return "Link Frequency";
  case ((0x009f0000 | 0x900) + 2):
    return "Pixel Rate";
  case ((0x009f0000 | 0x900) + 3):
    return "Test Pattern";
  case ((0x009f0000 | 0x900) + 4):
    return "Deinterlacing Mode";
  case ((0x009f0000 | 0x900) + 5):
    return "Digital Gain";

  case (0x00a00000 | 1):
    return "Digital Video Controls";
  case ((0x00a00000 | 0x900) + 1):
    return "Hotplug Present";
  case ((0x00a00000 | 0x900) + 2):
    return "RxSense Present";
  case ((0x00a00000 | 0x900) + 3):
    return "EDID Present";
  case ((0x00a00000 | 0x900) + 4):
    return "Transmit Mode";
  case ((0x00a00000 | 0x900) + 5):
    return "Tx RGB Quantization Range";
  case ((0x00a00000 | 0x900) + 6):
    return "Tx IT Content Type";
  case ((0x00a00000 | 0x900) + 100):
    return "Power Present";
  case ((0x00a00000 | 0x900) + 101):
    return "Rx RGB Quantization Range";
  case ((0x00a00000 | 0x900) + 102):
    return "Rx IT Content Type";

  case (0x00a10000 | 1):
    return "FM Radio Receiver Controls";
  case ((0x00a10000 | 0x900) + 1):
    return "De-Emphasis";
  case ((0x00a10000 | 0x900) + 2):
    return "RDS Reception";
  case (0x00a20000 | 1):
    return "RF Tuner Controls";
  case ((0x00a20000 | 0x900) + 32):
    return "RF Gain";
  case ((0x00a20000 | 0x900) + 41):
    return "LNA Gain, Auto";
  case ((0x00a20000 | 0x900) + 42):
    return "LNA Gain";
  case ((0x00a20000 | 0x900) + 51):
    return "Mixer Gain, Auto";
  case ((0x00a20000 | 0x900) + 52):
    return "Mixer Gain";
  case ((0x00a20000 | 0x900) + 61):
    return "IF Gain, Auto";
  case ((0x00a20000 | 0x900) + 62):
    return "IF Gain";
  case ((0x00a20000 | 0x900) + 11):
    return "Bandwidth, Auto";
  case ((0x00a20000 | 0x900) + 12):
    return "Bandwidth";
  case ((0x00a20000 | 0x900) + 91):
    return "PLL Lock";
  case ((0x00a10000 | 0x900) + 3):
    return "RDS Program Type";
  case ((0x00a10000 | 0x900) + 4):
    return "RDS PS Name";
  case ((0x00a10000 | 0x900) + 5):
    return "RDS Radio Text";
  case ((0x00a10000 | 0x900) + 6):
    return "RDS Traffic Announcement";
  case ((0x00a10000 | 0x900) + 7):
    return "RDS Traffic Program";
  case ((0x00a10000 | 0x900) + 8):
    return "RDS Music";

  case (0x00a30000 | 1):
    return "Detection Controls";
  case ((0x00a30000 | 0x900) + 1):
    return "Motion Detection Mode";
  case ((0x00a30000 | 0x900) + 2):
    return "MD Global Threshold";
  case ((0x00a30000 | 0x900) + 3):
    return "MD Threshold Grid";
  case ((0x00a30000 | 0x900) + 4):
    return "MD Region Grid";
  default:
    return (const char *) ((void *)0);
  }
}

void v4l2_ctrl_fill(u32 id, const char **name, enum v4l2_ctrl_type *type,
                    s64 *min, s64 *max, u64 *step, s64 *def, u32 *flags) {
  *name = v4l2_ctrl_get_name(id);
  *flags = 0;

  switch (id) {
  case ((0x00980000 | 0x900) + 9):
  case ((0x00980000 | 0x900) + 10):
  case ((0x00980000 | 0x900) + 12):
  case ((0x00980000 | 0x900) + 18):
  case ((0x00980000 | 0x900) + 20):
  case ((0x00980000 | 0x900) + 21):
  case ((0x00980000 | 0x900) + 25):
  case ((0x00980000 | 0x900) + 29):
  case ((0x00980000 | 0x900) + 30):
  case ((0x00980000 | 0x900) + 32):
  case ((0x00990000 | 0x900) + 109):
  case ((0x00990000 | 0x900) + 210):
  case ((0x00990000 | 0x900) + 204):
  case ((0x00990000 | 0x900) + 205):
  case ((0x009a0000 | 0x900) + 3):
  case ((0x009a0000 | 0x900) + 12):
  case ((0x009a0000 | 0x900) + 16):
  case ((0x009b0000 | 0x900) + 64):
  case ((0x009b0000 | 0x900) + 80):
  case ((0x009b0000 | 0x900) + 96):
  case ((0x00980000 | 0x900) + 37):
  case ((0x00980000 | 0x900) + 38):
  case ((0x009c0000 | 0x900) + 5):
  case ((0x009c0000 | 0x900) + 11):
  case ((0x009c0000 | 0x900) + 12):
  case ((0x00990000 | 0x900) + 213):
  case ((0x00990000 | 0x900) + 212):
  case ((0x00990000 | 0x900) + 215):
  case ((0x00990000 | 0x900) + 218):
  case ((0x00990000 | 0x900) + 355):
  case ((0x00990000 | 0x900) + 366):
  case ((0x00990000 | 0x900) + 407):
  case ((0x00990000 | 0x900) + 226):
  case ((0x009a0000 | 0x900) + 21):
  case ((0x009a0000 | 0x900) + 22):
  case ((0x00a10000 | 0x900) + 2):
  case ((0x00a20000 | 0x900) + 41):
  case ((0x00a20000 | 0x900) + 51):
  case ((0x00a20000 | 0x900) + 61):
  case ((0x00a20000 | 0x900) + 11):
  case ((0x00a20000 | 0x900) + 91):
  case ((0x009b0000 | 0x900) + 7):
  case ((0x009b0000 | 0x900) + 8):
  case ((0x009b0000 | 0x900) + 9):
  case ((0x009b0000 | 0x900) + 10):
  case ((0x009b0000 | 0x900) + 11):
  case ((0x009b0000 | 0x900) + 12):
  case ((0x009b0000 | 0x900) + 13):
  case ((0x009b0000 | 0x900) + 14):
  case ((0x00a10000 | 0x900) + 6):
  case ((0x00a10000 | 0x900) + 7):
  case ((0x00a10000 | 0x900) + 8):
    *type = V4L2_CTRL_TYPE_BOOLEAN;
    *min = 0;
    *max = *step = 1;
    break;
  case ((0x00980000 | 0x900) + 34):
    *type = V4L2_CTRL_TYPE_INTEGER;
    *flags |= 0x0400;
    break;
  case ((0x00990000 | 0x900) + 227):
  case ((0x00990000 | 0x900) + 228):
    *type = V4L2_CTRL_TYPE_INTEGER;
    break;
  case ((0x00990000 | 0x900) + 229):
  case ((0x009a0000 | 0x900) + 6):
  case ((0x009a0000 | 0x900) + 7):
  case ((0x009c0000 | 0x900) + 3):
  case ((0x009c0000 | 0x900) + 4):
  case ((0x009a0000 | 0x900) + 28):
  case ((0x009a0000 | 0x900) + 29):
  case ((0x00980000 | 0x900) + 13):
    *type = V4L2_CTRL_TYPE_BUTTON;
    *flags |= 0x0040 | 0x0200;
    *min = *max = *step = *def = 0;
    break;
  case ((0x00980000 | 0x900) + 24):
  case ((0x00990000 | 0x900) + 100):
  case ((0x00990000 | 0x900) + 101):
  case ((0x00990000 | 0x900) + 102):
  case ((0x00990000 | 0x900) + 103):
  case ((0x00990000 | 0x900) + 104):
  case ((0x00990000 | 0x900) + 111):
  case ((0x00990000 | 0x900) + 105):
  case ((0x00990000 | 0x900) + 106):
  case ((0x00990000 | 0x900) + 107):
  case ((0x00990000 | 0x900) + 108):
  case ((0x00990000 | 0x900) + 112):
  case ((0x00990000 | 0x900) + 113):
  case ((0x00990000 | 0x900) + 200):
  case ((0x00990000 | 0x900) + 201):
  case ((0x00990000 | 0x900) + 206):
  case ((0x00990000 | 0x900) + 0):
  case ((0x00990000 | 0x900) + 7):
  case ((0x009a0000 | 0x900) + 1):
  case ((0x009a0000 | 0x900) + 31):
  case ((0x00980000 | 0x900) + 31):
  case ((0x009a0000 | 0x900) + 20):
  case ((0x009b0000 | 0x900) + 112):
  case ((0x009c0000 | 0x900) + 1):
  case ((0x009c0000 | 0x900) + 2):
  case ((0x00990000 | 0x900) + 216):
  case ((0x00990000 | 0x900) + 221):
  case ((0x00990000 | 0x900) + 357):
  case ((0x00990000 | 0x900) + 359):
  case ((0x00990000 | 0x900) + 362):
  case ((0x00990000 | 0x900) + 363):
  case ((0x00990000 | 0x900) + 367):
  case ((0x00990000 | 0x900) + 370):
  case ((0x00990000 | 0x900) + 372):
  case ((0x00990000 | 0x900) + 1005):
  case ((0x00990000 | 0x900) + 1006):
  case ((0x00990000 | 0x900) + 270):
  case ((0x00990000 | 0x900) + 271):
  case ((0x00990000 | 0x900) + 405):
  case ((0x00990000 | 0x900) + 406):
  case ((0x009d0000 | 0x900) + 1):
  case ((0x009a0000 | 0x900) + 24):
  case ((0x009a0000 | 0x900) + 25):
  case ((0x009a0000 | 0x900) + 26):
  case ((0x00a00000 | 0x900) + 4):
  case ((0x00a00000 | 0x900) + 5):
  case ((0x00a00000 | 0x900) + 6):
  case ((0x00a00000 | 0x900) + 101):
  case ((0x00a00000 | 0x900) + 102):
  case ((0x009f0000 | 0x900) + 3):
  case ((0x009f0000 | 0x900) + 4):
  case ((0x00a10000 | 0x900) + 1):
  case ((0x00990000 | 0x900) + 506):
  case ((0x00990000 | 0x900) + 511):
  case ((0x00990000 | 0x900) + 512):
  case ((0x00a30000 | 0x900) + 1):
  case ((0x00990000 | 0x900) + 615):
  case ((0x00990000 | 0x900) + 616):
  case ((0x00990000 | 0x900) + 606):
  case ((0x00990000 | 0x900) + 623):
  case ((0x00990000 | 0x900) + 635):
  case ((0x00990000 | 0x900) + 618):
  case ((0x00990000 | 0x900) + 620):
  case ((0x00990000 | 0x900) + 1015):
  case ((0x00990000 | 0x900) + 1016):
  case ((0x009a0000 | 0x900) + 34):
    *type = V4L2_CTRL_TYPE_MENU;
    break;
  case ((0x009f0000 | 0x900) + 1):
    *type = V4L2_CTRL_TYPE_INTEGER_MENU;
    break;
  case ((0x009b0000 | 0x900) + 5):
  case ((0x009b0000 | 0x900) + 6):
  case ((0x00a10000 | 0x900) + 4):
  case ((0x00a10000 | 0x900) + 5):
    *type = V4L2_CTRL_TYPE_STRING;
    break;
  case ((0x009a0000 | 0x900) + 23):
  case ((0x009a0000 | 0x900) + 19):
  case ((0x00990000 | 0x900) + 500):
  case ((0x00990000 | 0x900) + 502):
    *type = V4L2_CTRL_TYPE_INTEGER_MENU;
    break;
  case (0x00980000 | 1):
  case (0x009a0000 | 1):
  case (0x00990000 | 1):
  case (0x009b0000 | 1):
  case (0x009c0000 | 1):
  case (0x009d0000 | 1):
  case (0x009e0000 | 1):
  case (0x009f0000 | 1):
  case (0x00a00000 | 1):
  case (0x00a10000 | 1):
  case (0x00a20000 | 1):
  case (0x00a30000 | 1):
    *type = V4L2_CTRL_TYPE_CTRL_CLASS;

    *flags |= 0x0004 | 0x0040;
    *min = *max = *step = *def = 0;
    break;
  case ((0x00980000 | 0x900) + 35):
    *type = V4L2_CTRL_TYPE_INTEGER;
    *step = 1;
    *min = 0;

    *max = 0xFFFFFF;
    break;
  case ((0x009c0000 | 0x900) + 10):
  case ((0x009d0000 | 0x900) + 4):
  case ((0x009a0000 | 0x900) + 27):
  case ((0x009a0000 | 0x900) + 30):
  case ((0x00a00000 | 0x900) + 1):
  case ((0x00a00000 | 0x900) + 2):
  case ((0x00a00000 | 0x900) + 3):
  case ((0x00a00000 | 0x900) + 100):
    *type = V4L2_CTRL_TYPE_BITMASK;
    break;
  case ((0x00980000 | 0x900) + 39):
  case ((0x00980000 | 0x900) + 40):
    *type = V4L2_CTRL_TYPE_INTEGER;
    *flags |= 0x0004;
    break;
  case ((0x00990000 | 0x900) + 223):
    *type = V4L2_CTRL_TYPE_INTEGER64;
    *flags |= 0x0080 | 0x0004;
    *min = *def = 0;
    *max = 0x1ffffffffLL;
    *step = 1;
    break;
  case ((0x00990000 | 0x900) + 224):
    *type = V4L2_CTRL_TYPE_INTEGER64;
    *flags |= 0x0080 | 0x0004;
    *min = *def = 0;
    *max = 0x7fffffffffffffffLL;
    *step = 1;
    break;
  case ((0x009f0000 | 0x900) + 2):
    *type = V4L2_CTRL_TYPE_INTEGER64;
    *flags |= 0x0004;
    break;
  case ((0x00a30000 | 0x900) + 4):
    *type = V4L2_CTRL_TYPE_U8;
    break;
  case ((0x00a30000 | 0x900) + 3):
    *type = V4L2_CTRL_TYPE_U16;
    break;
  case ((0x009b0000 | 0x900) + 15):
    *type = V4L2_CTRL_TYPE_U32;
    break;
  case ((0x00990000 | 0x900) + 250):
    *type = (enum v4l2_ctrl_type) 0x0103;
    break;
  case ((0x00990000 | 0x900) + 251):
    *type = (enum v4l2_ctrl_type) 0x0104;
    break;
  case ((0x00990000 | 0x900) + 292):
    *type = (enum v4l2_ctrl_type) 0x0105;
    break;
  case ((0x00990000 | 0x900) + 1000):
    *type = (enum v4l2_ctrl_type) 0x0110;
    break;
  case ((0x00990000 | 0x900) + 1001):
    *type = (enum v4l2_ctrl_type) 0x0111;
    break;
  case ((0x00990000 | 0x900) + 1002):
    *type = (enum v4l2_ctrl_type) 0x0112;
    break;
  case ((0x00990000 | 0x900) + 1003):
    *type = (enum v4l2_ctrl_type) 0x0113;
    break;
  case ((0x00990000 | 0x900) + 1004):
    *type = (enum v4l2_ctrl_type) 0x0114;
    break;
  case ((0x00990000 | 0x900) + 2000):
    *type = (enum v4l2_ctrl_type) 0x301;
    break;
  case ((0x00990000 | 0x900) + 1008):
    *type = (enum v4l2_ctrl_type) 0x0120;
    break;
  case ((0x00990000 | 0x900) + 1009):
    *type = (enum v4l2_ctrl_type) 0x0121;
    break;
  case ((0x00990000 | 0x900) + 1010):
    *type = (enum v4l2_ctrl_type) 0x0122;
    break;
  case ((0x009e0000 | 0x900) + 8):
    *type = V4L2_CTRL_TYPE_AREA;
    *flags |= 0x0004;
    break;
  default:
    *type = V4L2_CTRL_TYPE_INTEGER;
    break;
  }
  switch (id) {
  case ((0x00990000 | 0x900) + 101):
  case ((0x00990000 | 0x900) + 105):
  case ((0x00990000 | 0x900) + 206):
  case ((0x00990000 | 0x900) + 202):
  case ((0x00990000 | 0x900) + 0):
    *flags |= 0x0008;
    break;
  case ((0x00980000 | 0x900) + 5):
  case ((0x00980000 | 0x900) + 6):
  case ((0x00980000 | 0x900) + 7):
  case ((0x00980000 | 0x900) + 8):
  case ((0x00980000 | 0x900) + 0):
  case ((0x00980000 | 0x900) + 1):
  case ((0x00980000 | 0x900) + 2):
  case ((0x00980000 | 0x900) + 3):
  case ((0x00980000 | 0x900) + 14):
  case ((0x00980000 | 0x900) + 15):
  case ((0x00980000 | 0x900) + 16):
  case ((0x00980000 | 0x900) + 27):
  case ((0x00980000 | 0x900) + 36):
  case ((0x009b0000 | 0x900) + 1):
  case ((0x009b0000 | 0x900) + 65):
  case ((0x009b0000 | 0x900) + 66):
  case ((0x009b0000 | 0x900) + 81):
  case ((0x009b0000 | 0x900) + 82):
  case ((0x009b0000 | 0x900) + 83):
  case ((0x009b0000 | 0x900) + 84):
  case ((0x009b0000 | 0x900) + 97):
  case ((0x009b0000 | 0x900) + 98):
  case ((0x009b0000 | 0x900) + 113):
  case ((0x009b0000 | 0x900) + 114):
  case ((0x00a20000 | 0x900) + 32):
  case ((0x00a20000 | 0x900) + 42):
  case ((0x00a20000 | 0x900) + 52):
  case ((0x00a20000 | 0x900) + 62):
  case ((0x00a20000 | 0x900) + 12):
  case ((0x00a30000 | 0x900) + 2):
    *flags |= 0x0020;
    break;
  case ((0x009a0000 | 0x900) + 4):
  case ((0x009a0000 | 0x900) + 5):
  case ((0x009a0000 | 0x900) + 11):
  case ((0x009a0000 | 0x900) + 18):
  case ((0x009a0000 | 0x900) + 14):
    *flags |= 0x0040 | 0x0200;
    break;
  case ((0x009c0000 | 0x900) + 5):
  case ((0x009a0000 | 0x900) + 30):
  case ((0x009c0000 | 0x900) + 12):
  case ((0x00a00000 | 0x900) + 1):
  case ((0x00a00000 | 0x900) + 2):
  case ((0x00a00000 | 0x900) + 3):
  case ((0x00a00000 | 0x900) + 100):
  case ((0x00a00000 | 0x900) + 102):
  case ((0x00a10000 | 0x900) + 3):
  case ((0x00a10000 | 0x900) + 4):
  case ((0x00a10000 | 0x900) + 5):
  case ((0x00a10000 | 0x900) + 6):
  case ((0x00a10000 | 0x900) + 7):
  case ((0x00a10000 | 0x900) + 8):
  case ((0x009a0000 | 0x900) + 34):
  case ((0x009a0000 | 0x900) + 35):
    *flags |= 0x0004;
    break;
  case ((0x00a20000 | 0x900) + 91):
    *flags |= 0x0080;
    break;
  }
}
