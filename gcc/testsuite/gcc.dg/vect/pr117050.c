/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

typedef struct {
  char *data;
} song_sample_t;
typedef struct {
  int right_ramp;
  int left_ramp;
} song_voice_t;
song_sample_t *csf_stop_sample_smp, *csf_stop_sample_v_3;
song_voice_t *csf_stop_sample_v;
void csf_stop_sample()
{
  for (int i; i; i++, csf_stop_sample_v++)
    if (csf_stop_sample_v_3 || csf_stop_sample_smp->data)
      csf_stop_sample_v->left_ramp = csf_stop_sample_v->right_ramp = 0;
}
