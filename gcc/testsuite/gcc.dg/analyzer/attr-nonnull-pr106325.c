typedef long int signed_frame_t;

typedef struct Track Track;
typedef struct ZRegion ZRegion;
typedef struct AutomationTrack AutomationTrack;
typedef struct MidiNote MidiNote;
typedef struct ArrangerObject ArrangerObject;
typedef struct Project Project;
typedef struct ZRegion ZRegion;
typedef struct Position Position;
typedef struct Track Track;
typedef struct ClipEditor ClipEditor;

typedef enum ArrangerObjectType
{
  /* .... */
  ARRANGER_OBJECT_TYPE_REGION,
  ARRANGER_OBJECT_TYPE_MIDI_NOTE,
  /* .... */
} ArrangerObjectType;

typedef enum ArrangerObjectPositionType
{
  ARRANGER_OBJECT_POSITION_TYPE_START,
  ARRANGER_OBJECT_POSITION_TYPE_END,
  ARRANGER_OBJECT_POSITION_TYPE_CLIP_START,
  ARRANGER_OBJECT_POSITION_TYPE_LOOP_START,
  ARRANGER_OBJECT_POSITION_TYPE_LOOP_END,
  ARRANGER_OBJECT_POSITION_TYPE_FADE_IN,
  ARRANGER_OBJECT_POSITION_TYPE_FADE_OUT,
} ArrangerObjectPositionType;

typedef struct Position
{
  /* .... */
  double ticks;
  /* .... */
} Position;

typedef enum RegionType
{
  /* .... */
  REGION_TYPE_AUTOMATION = 1 << 2,
  /* .... */
} RegionType;

typedef struct RegionIdentifier
{
  /* .... */
  RegionType type;
  /* .... */
  int lane_pos;
  /* .... */
} RegionIdentifier;

typedef struct ArrangerObject
{
  /* .... */

  ArrangerObjectType type;
  /* .... */
  Position pos;
  Position end_pos;
  Position clip_start_pos;

  Position loop_start_pos;
  Position loop_end_pos;

  Position fade_in_pos;
  Position fade_out_pos;

  /* .... */
} ArrangerObject;

typedef struct ZRegion
{
  /* .... */
  RegionIdentifier id;
  /* .... */
  int num_midi_notes;
  /* .... */
} ZRegion;

typedef struct Zrythm
{
  /* ... */
  Project *project;
  /* ... */
} Zrythm;

typedef struct Project
{
  /* ... */

  ClipEditor *clip_editor;

  /* ... */
} Project;

extern Zrythm *zrythm;

extern void g_return_if_fail_warning (const char *log_domain,
                                      const char *pretty_function,
                                      const char *expression);
extern void position_add_ticks (Position *self, double ticks);
extern _Bool
arranger_object_is_position_valid (const ArrangerObject *const self,
                                   const Position *pos,
                                   ArrangerObjectPositionType pos_type);
extern Track *arranger_object_get_track (const ArrangerObject *const self);
extern void midi_region_insert_midi_note (ZRegion *region, MidiNote *midi_note,
                                          int idx, int pub_events);
extern ZRegion *midi_note_get_region (MidiNote *self);
extern AutomationTrack *
region_get_automation_track (const ZRegion *const region);
extern void track_add_region (Track *track, ZRegion *region,
                              AutomationTrack *at, int lane_pos, int gen_name,
                              int fire_events);
extern void clip_editor_set_region (ClipEditor *self, ZRegion *region,
                                    _Bool fire_events);
extern ZRegion *clip_editor_get_region (ClipEditor *self);

static Position *
get_position_ptr (ArrangerObject *self, ArrangerObjectPositionType pos_type)
{
  switch (pos_type)
    {
    case ARRANGER_OBJECT_POSITION_TYPE_START:
      return &self->pos;
    case ARRANGER_OBJECT_POSITION_TYPE_END:
      return &self->end_pos;
    case ARRANGER_OBJECT_POSITION_TYPE_CLIP_START:
      return &self->clip_start_pos;
    case ARRANGER_OBJECT_POSITION_TYPE_LOOP_START:
      return &self->loop_start_pos;
    case ARRANGER_OBJECT_POSITION_TYPE_LOOP_END:
      return &self->loop_end_pos;
    case ARRANGER_OBJECT_POSITION_TYPE_FADE_IN:
      return &self->fade_in_pos;
    case ARRANGER_OBJECT_POSITION_TYPE_FADE_OUT:
      return &self->fade_out_pos;
    }
  return (((void *)0));
}

void
arranger_object_set_position (ArrangerObject *self, const Position *pos,
                              ArrangerObjectPositionType pos_type,
                              const int validate)
{
  if (!(self && pos))
    {
      g_return_if_fail_warning ("zrythm", ((const char *)(__func__)),
                                "self && pos");
      return;
    }

  if (validate && !arranger_object_is_position_valid (self, pos, pos_type))
    return;

  Position *pos_ptr;
  pos_ptr = get_position_ptr (self, pos_type);
  if (!pos_ptr)
    {
      g_return_if_fail_warning ("zrythm", ((const char *)(__func__)),
                                "pos_ptr");
      return;
    }
  *(pos_ptr) = *(pos);
}

void
arranger_object_end_pos_setter (ArrangerObject *self, const Position *pos)
{
  arranger_object_set_position (self, pos, ARRANGER_OBJECT_POSITION_TYPE_END,
                                1);
}

ArrangerObject *
arranger_object_clone (const ArrangerObject *self)
{
  if (!self)
    {
      g_return_if_fail_warning ("zrythm", ((const char *)(__func__)), "self");
      return (((void *)0));
    }
  /* .... */
  return (((void *)0));
}

__attribute__((nonnull(1, 2)))
void
arranger_object_unsplit (ArrangerObject *r1, ArrangerObject *r2,
                         ArrangerObject **obj, _Bool fire_events)
{
  ZRegion *clip_editor_region
      = clip_editor_get_region (((zrythm)->project->clip_editor));

  _Bool set_clip_editor_region = 0;
  if (clip_editor_region == (ZRegion *)r1
      || clip_editor_region == (ZRegion *)r2)
    {
      set_clip_editor_region = 1;
      clip_editor_set_region (((zrythm)->project->clip_editor), ((void *)0),
                              1);
    }

  *obj = arranger_object_clone (r1);

  arranger_object_end_pos_setter (*obj, &r2->end_pos);
  Position fade_out_pos;
  *(&fade_out_pos) = *(&r2->end_pos);
  position_add_ticks (&fade_out_pos, -r2->pos.ticks);
  arranger_object_set_position (*obj, &fade_out_pos,
                                ARRANGER_OBJECT_POSITION_TYPE_FADE_OUT, 0);

  switch (r1->type) /* { dg-bogus "dereference of NULL 'r1'" } */
    {
    case ARRANGER_OBJECT_TYPE_REGION:
      {
        ZRegion *r1_region = (ZRegion *)r1;
        AutomationTrack *at = ((void *)0);
        if (r1_region->id.type == REGION_TYPE_AUTOMATION)
          {
            at = region_get_automation_track (r1_region);
          }
        track_add_region (arranger_object_get_track (r1), (ZRegion *)*obj, at,
                          ((ZRegion *)r1)->id.lane_pos, 1, fire_events);
      }
      break;
    case ARRANGER_OBJECT_TYPE_MIDI_NOTE:
      {
        ZRegion *parent_region = midi_note_get_region (((MidiNote *)r1));
        midi_region_insert_midi_note (
            parent_region, (MidiNote *)*obj,
            ((ZRegion *)(parent_region))->num_midi_notes, 1);
      }
      break;
    default:
      break;
    }

  switch (r1->type) /* { dg-bogus "dereference of NULL 'r1'" } */
    {
    /* .... */
    default:
      break;
    }
  /* .... */
}
