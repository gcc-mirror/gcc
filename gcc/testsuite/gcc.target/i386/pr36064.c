/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -march=core2" } */

typedef long long ogg_int64_t;

typedef struct vorbis_info
{
  long rate;
} vorbis_info;

typedef struct OggVorbis_File
{
  int seekable;
  int links;
  ogg_int64_t *pcmlengths;
  vorbis_info *vi;
  int ready_state;
} OggVorbis_File;

extern double ov_time_total (OggVorbis_File * vf, int i);
extern int ov_pcm_seek_page (OggVorbis_File * vf, ogg_int64_t pos);

int
ov_time_seek_page (OggVorbis_File * vf, double seconds)
{
  int link = -1;
  ogg_int64_t pcm_total = 0;
  double time_total = 0.;

  if (vf->ready_state < 2)
    return (-131);
  if (!vf->seekable)
    return (-138);
  if (seconds < 0)
    return (-131);

  for (link = 0; link < vf->links; link++)
    {
      double addsec = ov_time_total (vf, link);
      if (seconds < time_total + addsec)
	break;
      time_total += addsec;
      pcm_total += vf->pcmlengths[link * 2 + 1];
    }

  if (link == vf->links)
    return (-131);

  {
    ogg_int64_t target =
      pcm_total + (seconds - time_total) * vf->vi[link].rate;
    return (ov_pcm_seek_page (vf, target));
  }
}
