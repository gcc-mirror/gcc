/* { dg-do compile } */
/* { dg-options "-w -O2 -g -fselective-scheduling2" } */

typedef long unsigned int size_t;
struct fileloc
{
  const char *file;
};
typedef struct type *type_p;
typedef const struct type *const_type_p;
enum typekind
{
  TYPE_STRUCT,
  TYPE_UNION,
  TYPE_POINTER,
  TYPE_LANG_STRUCT,
  TYPE_PARAM_STRUCT
};
struct type
{
  enum typekind kind;
  union
  {
    struct
    {
      struct fileloc line;
    } s;
    struct
    {
      struct fileloc line;
    } param_struct;
  } u;
};
struct outf
{
  size_t bufused;
  char *buf;
};
typedef struct outf *outf_p;
oprintf (outf_p o, const char *format, ...)
{
  char *s;
  size_t slength;
  memcpy (o->buf + o->bufused, s, slength);
}
output_mangled_typename (outf_p of, const_type_p t)
{
    switch (t->kind)
      {
      case TYPE_POINTER: (fancy_abort ("/gcc/gengtype.c", 1988, __FUNCTION__));
    }
}
output_type_enum (outf_p of, type_p s)
{
  if (s->kind == TYPE_PARAM_STRUCT && s->u.param_struct.line.file != ((void *)0))
    {
      oprintf (of, ", gt_e_");
    }
  else if (((s)->kind == TYPE_UNION || (s)->kind == TYPE_STRUCT || (s)->kind == TYPE_LANG_STRUCT) && s->u.s.line.file != ((void *)0))
    {
      oprintf (of, ", gt_ggc_e_");
      output_mangled_typename (of, s);
    }
  else
    oprintf (of, ", gt_types_enum_last");
}
