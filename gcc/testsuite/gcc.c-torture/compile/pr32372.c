typedef struct AVCodecContext
{
  int flags;
  void *priv_data;
  char codec_name[32];
}
AVCodecContext;
typedef struct ScanTable
{
  int obmc;
  int umvplus;
  int h263_aic;
}
MpegEncContext;
void
MPV_encode_init (AVCodecContext *avctx)
{
  MpegEncContext *s = avctx->priv_data;
  s->umvplus = (avctx->flags & 0x02000000) ? 1 : 0;
  s->h263_aic = (avctx->flags & 0x01000000) ? 1 : 0;
  s->h263_aic = s->obmc || s->umvplus;
}
