/* Reduced from Doom's linuxdoom-1.10/s_sound.c, which is GPLv2 or later.  */

/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

typedef struct _IO_FILE FILE;
extern FILE* stderr;
extern int
fprintf(FILE* __restrict __stream, const char* __restrict __format, ...);
extern int
sprintf(char* __restrict __s, const char* __restrict __format, ...)
  __attribute__((__nothrow__));
extern int
abs(int __x) __attribute__((__nothrow__, __leaf__)) __attribute__((__const__));

typedef enum
{
  false,
  true
} boolean;

typedef unsigned char byte;

void
I_Error(char* error, ...);

typedef enum
{
  shareware,
  registered,
  commercial,
  /* [...snip...] */
} GameMode_t;

typedef int fixed_t;

fixed_t
FixedMul(fixed_t a, fixed_t b);

extern fixed_t finesine[5 * 8192 / 4];
typedef unsigned angle_t;

typedef struct mobj_s
{
  /* [...snip...] */
  fixed_t x;
  fixed_t y;
  fixed_t z;
  /* [...snip...] */
  angle_t angle;
  /* [...snip...] */
} mobj_t;

typedef struct player_s
{
  mobj_t* mo;
  /* [...snip...] */
} player_t;

extern GameMode_t gamemode;
extern int gameepisode;
extern int gamemap;
extern int consoleplayer;
extern player_t players[4];

typedef struct sfxinfo_struct sfxinfo_t;

struct sfxinfo_struct
{
  /* [...snip...] */
  int priority;
  sfxinfo_t* link;
  int pitch;
  int volume;
  void* data;
  int usefulness;
  int lumpnum;
};

typedef struct
{
  char* name;
  int lumpnum;
  void* data;
  int handle;
} musicinfo_t;

extern sfxinfo_t S_sfx[];

extern musicinfo_t S_music[];

typedef enum
{
  mus_None,
  mus_e1m1,
  /* [...snip...] */
  mus_e1m5,
  /* [...snip...] */
  mus_e1m9,
  /* [...snip...] */
  mus_e2m4,
  mus_e2m5,
  mus_e2m6,
  mus_e2m7,
  /* [...snip...] */
  mus_e3m2,
  mus_e3m3,
  mus_e3m4,
  /* [...snip...] */
  mus_runnin,
  /* [...snip...] */
  NUMMUSIC
} musicenum_t;

typedef enum
{
  /* [...snip...] */
  sfx_sawup,
  /* [...snip...] */
  sfx_sawhit,
  /* [...snip...] */
  sfx_itemup,
  /* [...snip...] */
  sfx_tink,
  /* [...snip...] */
  NUMSFX
} sfxenum_t;


void
I_SetChannels();

int
I_GetSfxLumpNum(sfxinfo_t* sfxinfo);

int
I_StartSound(int id, int vol, int sep, int pitch, int priority);

void
I_StopSound(int handle);
int
I_SoundIsPlaying(int handle);
void
I_UpdateSoundParams(int handle, int vol, int sep, int pitch);

void
I_SetMusicVolume(int volume);

void
I_PauseSong(int handle);
void
I_ResumeSong(int handle);
int
I_RegisterSong(void* data);

void
I_PlaySong(int handle, int looping);

void
I_StopSong(int handle);

void
I_UnRegisterSong(int handle);
void
S_StopSound(void* origin);
void
S_ChangeMusic(int music_id, int looping);
void
S_StopMusic(void);

void
S_SetMusicVolume(int volume);
void
S_SetSfxVolume(int volume);

void*
Z_Malloc(int size, int tag, void* ptr);
void
Z_ChangeTag2(void* ptr, int tag);

typedef struct memblock_s
{
  /* [...snip...] */
  int id;
  /* [...snip...] */
} memblock_t;
int
M_Random(void);
int
W_GetNumForName(char* name);
void*
W_CacheLumpNum(int lump, int tag);
angle_t
R_PointToAngle2(fixed_t x1, fixed_t y1, fixed_t x2, fixed_t y2);

typedef struct
{
  sfxinfo_t* sfxinfo;
  void* origin;
  int handle;
} channel_t;
static channel_t* channels;

int snd_SfxVolume = 15;
int snd_MusicVolume = 15;
static boolean mus_paused;
static musicinfo_t* mus_playing = 0;
int numChannels;
static int nextcleanup;

int
S_getChannel(void* origin, sfxinfo_t* sfxinfo);

int
S_AdjustSoundParams(mobj_t* listener,
                    mobj_t* source,
                    int* vol,
                    int* sep,
                    int* pitch);
void
S_StopChannel(int cnum);

void
S_Init(int sfxVolume, int musicVolume)
{
  int i;

  fprintf(stderr, "S_Init: default sfx volume %d\n", sfxVolume);

  I_SetChannels();

  S_SetSfxVolume(sfxVolume);

  S_SetMusicVolume(musicVolume);

  channels = (channel_t*)Z_Malloc(numChannels * sizeof(channel_t), 1, 0);

  for (i = 0; i < numChannels; i++)
    channels[i].sfxinfo = 0;

  mus_paused = 0;

  for (i = 1; i < NUMSFX; i++)
    S_sfx[i].lumpnum = S_sfx[i].usefulness = -1;
}
void
S_Start(void)
{
  int cnum;
  int mnum;

  for (cnum = 0; cnum < numChannels; cnum++)
    if (channels[cnum].sfxinfo)
      S_StopChannel(cnum);

  mus_paused = 0;

  if (gamemode == commercial)
    mnum = mus_runnin + gamemap - 1;
  else {
    int spmus[] = {

      mus_e3m4, mus_e3m2, mus_e3m3, mus_e1m5, mus_e2m7,
      mus_e2m4, mus_e2m6, mus_e2m5, mus_e1m9
    };

    if (gameepisode < 4)
      mnum = mus_e1m1 + (gameepisode - 1) * 9 + gamemap - 1;
    else
      mnum = spmus[gamemap - 1];
  }

  S_ChangeMusic(mnum, true);

  nextcleanup = 15;
}

void
S_StartSoundAtVolume(void* origin_p, int sfx_id, int volume)
{

  int rc;
  int sep;
  int pitch;
  int priority;
  sfxinfo_t* sfx;
  int cnum;

  mobj_t* origin = (mobj_t*)origin_p;
  if (sfx_id < 1 || sfx_id > NUMSFX)
    I_Error("Bad sfx #: %d", sfx_id);

  sfx = &S_sfx[sfx_id];

  if (sfx->link) {
    pitch = sfx->pitch;
    priority = sfx->priority;
    volume += sfx->volume;

    if (volume < 1)
      return;

    if (volume > snd_SfxVolume)
      volume = snd_SfxVolume;
  } else {
    pitch = 128;
    priority = 64;
  }

  if (origin && origin != players[consoleplayer].mo) {
    rc = S_AdjustSoundParams(
      players[consoleplayer].mo, origin, &volume, &sep, &pitch);

    if (origin->x == players[consoleplayer].mo->x &&
        origin->y == players[consoleplayer].mo->y) {
      sep = 128;
    }

    if (!rc)
      return;
  } else {
    sep = 128;
  }

  if (sfx_id >= sfx_sawup && sfx_id <= sfx_sawhit) {
    pitch += 8 - (M_Random() & 15);

    if (pitch < 0)
      pitch = 0;
    else if (pitch > 255)
      pitch = 255;
  } else if (sfx_id != sfx_itemup && sfx_id != sfx_tink) {
    pitch += 16 - (M_Random() & 31);

    if (pitch < 0)
      pitch = 0;
    else if (pitch > 255)
      pitch = 255;
  }

  S_StopSound(origin);

  cnum = S_getChannel(origin, sfx);

  if (cnum < 0)
    return;
  if (sfx->lumpnum < 0)
    sfx->lumpnum = I_GetSfxLumpNum(sfx);

  if (!sfx->data) {
    fprintf(stderr, "S_StartSoundAtVolume: 16bit and not pre-cached - wtf?\n");
  }

  if (sfx->usefulness++ < 0)
    sfx->usefulness = 1;

  channels[cnum].handle = I_StartSound(sfx_id,

                                       volume,
                                       sep,
                                       pitch,
                                       priority);
}

void
S_StartSound(void* origin, int sfx_id)
{

  S_StartSoundAtVolume(origin, sfx_id, snd_SfxVolume);
}

void
S_StopSound(void* origin)
{

  int cnum;

  for (cnum = 0; cnum < numChannels; cnum++) {
    if (channels[cnum].sfxinfo && channels[cnum].origin == origin) {
      S_StopChannel(cnum);
      break;
    }
  }
}
void
S_PauseSound(void)
{
  if (mus_playing && !mus_paused) {
    I_PauseSong(mus_playing->handle);
    mus_paused = true;
  }
}

void
S_ResumeSound(void)
{
  if (mus_playing && mus_paused) {
    I_ResumeSong(mus_playing->handle);
    mus_paused = false;
  }
}

void
S_UpdateSounds(void* listener_p)
{
  int audible;
  int cnum;
  int volume;
  int sep;
  int pitch;
  sfxinfo_t* sfx;
  channel_t* c;

  mobj_t* listener = (mobj_t*)listener_p;
  for (cnum = 0; cnum < numChannels; cnum++) {
    c = &channels[cnum];
    sfx = c->sfxinfo;

    if (c->sfxinfo) {
      if (I_SoundIsPlaying(c->handle)) {

        volume = snd_SfxVolume;
        pitch = 128;
        sep = 128;

        if (sfx->link) {
          pitch = sfx->pitch;
          volume += sfx->volume;
          if (volume < 1) {
            S_StopChannel(cnum);
            continue;
          } else if (volume > snd_SfxVolume) {
            volume = snd_SfxVolume;
          }
        }

        if (c->origin && listener_p != c->origin) {
          audible =
            S_AdjustSoundParams(listener, c->origin, &volume, &sep, &pitch);

          if (!audible) {
            S_StopChannel(cnum);
          } else
            I_UpdateSoundParams(c->handle, volume, sep, pitch);
        }
      } else {

        S_StopChannel(cnum);
      }
    }
  }
}

void
S_SetMusicVolume(int volume)
{
  if (volume < 0 || volume > 127) {
    I_Error("Attempt to set music volume at %d", volume);
  }

  I_SetMusicVolume(127);
  I_SetMusicVolume(volume);
  snd_MusicVolume = volume;
}

void
S_SetSfxVolume(int volume)
{

  if (volume < 0 || volume > 127)
    I_Error("Attempt to set sfx volume at %d", volume);

  snd_SfxVolume = volume;
}

void
S_StartMusic(int m_id)
{
  S_ChangeMusic(m_id, false);
}

void
S_ChangeMusic(int musicnum, int looping)
{
  musicinfo_t* music;
  char namebuf[9];

  if ((musicnum <= mus_None) || (musicnum >= NUMMUSIC)) {
    I_Error("Bad music number %d", musicnum);
  } else
    music = &S_music[musicnum];

  /* We don't know that I_Error exits, so actually a false positive;
     see PR analyzer/108867.  */

  if (mus_playing == music) /* { dg-warning "use of uninitialized value 'music'" } */
    return;

  S_StopMusic();

  /* We shouldn't issue further warnings about 'music' being
     uninitialized.  */

  if (!music->lumpnum) { /* { dg-bogus "use of uninitialized value 'music'" } */
    sprintf(namebuf, "d_%s", music->name); /* { dg-bogus "use of uninitialized value 'music'" } */
    music->lumpnum = W_GetNumForName(namebuf); /* { dg-bogus "use of uninitialized value 'music'" } */
  }

  music->data = (void*)W_CacheLumpNum(music->lumpnum, 3); /* { dg-bogus "use of uninitialized value 'music'" } */
  music->handle = I_RegisterSong(music->data); /* { dg-bogus "use of uninitialized value 'music'" } */

  I_PlaySong(music->handle, looping); /* { dg-bogus "use of uninitialized value 'music'" } */

  mus_playing = music; /* { dg-bogus "use of uninitialized value 'music'" } */
}

void
S_StopMusic(void)
{
  if (mus_playing) {
    if (mus_paused)
      I_ResumeSong(mus_playing->handle);

    I_StopSong(mus_playing->handle);
    I_UnRegisterSong(mus_playing->handle);
    {
      if (((memblock_t*)((byte*)(mus_playing->data) - sizeof(memblock_t)))
            ->id != 0x1d4a11)
        I_Error("Z_CT at "
                "s_sound.c"
                ":%i",
                699);
      Z_ChangeTag2(mus_playing->data, 101);
    };
    ;

    mus_playing->data = 0;
    mus_playing = 0;
  }
}

void
S_StopChannel(int cnum)
{

  int i;
  channel_t* c = &channels[cnum];

  if (c->sfxinfo) {

    if (I_SoundIsPlaying(c->handle)) {

      I_StopSound(c->handle);
    }

    for (i = 0; i < numChannels; i++) {
      if (cnum != i && c->sfxinfo == channels[i].sfxinfo) {
        break;
      }
    }

    c->sfxinfo->usefulness--;

    c->sfxinfo = 0;
  }
}
int
S_AdjustSoundParams(mobj_t* listener,
                    mobj_t* source,
                    int* vol,
                    int* sep,
                    int* pitch)
{
  fixed_t approx_dist;
  fixed_t adx;
  fixed_t ady;
  angle_t angle;

  adx = abs(listener->x - source->x);
  ady = abs(listener->y - source->y);

  approx_dist = adx + ady - ((adx < ady ? adx : ady) >> 1);

  if (gamemap != 8 && approx_dist > (1200 * 0x10000)) {
    return 0;
  }

  angle = R_PointToAngle2(listener->x, listener->y, source->x, source->y);

  if (angle > listener->angle)
    angle = angle - listener->angle;
  else
    angle = angle + (0xffffffff - listener->angle);

  angle >>= 19;

  *sep = 128 - (FixedMul((96 * 0x10000), finesine[angle]) >> 16);

  if (approx_dist < (160 * 0x10000)) {
    *vol = snd_SfxVolume;
  } else if (gamemap == 8) {
    if (approx_dist > (1200 * 0x10000))
      approx_dist = (1200 * 0x10000);

    *vol =
      15 + ((snd_SfxVolume - 15) * (((1200 * 0x10000) - approx_dist) >> 16)) /
             (((1200 * 0x10000) - (160 * 0x10000)) >> 16);
  } else {

    *vol = (snd_SfxVolume * (((1200 * 0x10000) - approx_dist) >> 16)) /
           (((1200 * 0x10000) - (160 * 0x10000)) >> 16);
  }

  return (*vol > 0);
}
int
S_getChannel(void* origin, sfxinfo_t* sfxinfo)
{

  int cnum;

  channel_t* c;

  for (cnum = 0; cnum < numChannels; cnum++) {
    if (!channels[cnum].sfxinfo)
      break;
    else if (origin && channels[cnum].origin == origin) {
      S_StopChannel(cnum);
      break;
    }
  }

  if (cnum == numChannels) {

    for (cnum = 0; cnum < numChannels; cnum++)
      if (channels[cnum].sfxinfo->priority >= sfxinfo->priority) /* { dg-warning "dereference of NULL" } */
        break;

    if (cnum == numChannels) {

      return -1;
    } else {

      S_StopChannel(cnum);
    }
  }

  c = &channels[cnum];

  c->sfxinfo = sfxinfo;
  c->origin = origin;

  return cnum;
}
