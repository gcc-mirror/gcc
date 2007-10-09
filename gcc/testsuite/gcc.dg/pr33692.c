/* { dg-do compile } */

/* We ICEd with type-checking enabled.  */

typedef struct { int i; } snd_pcm_info_t;
typedef struct { snd_pcm_info_t info; } snd_pcm_shm_ctrl_t;
void snd_pcm_info(snd_pcm_info_t *);
int pcm_shm_cmd(volatile snd_pcm_shm_ctrl_t *ctrl)
{
  snd_pcm_info((snd_pcm_info_t *) &ctrl->info);
}

