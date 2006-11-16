/* { dg-do assemble { target ia64-*-* } } */
typedef int gint;
typedef gint gboolean;
typedef unsigned int guint;
typedef struct _MetaRectangle MetaRectangle;
struct _MetaRectangle
{
  int x;
  int y;
};
typedef struct _MetaDisplay MetaDisplay;
typedef struct _MetaFrame MetaFrame;
typedef struct _MetaWindow MetaWindow;
typedef struct
{
  int win_gravity;
}
XSizeHints;
typedef enum
{
  META_DEBUG_FOCUS = 1 << 0, META_DEBUG_WORKAREA = 1 << 1, META_DEBUG_STACK =
    1 << 6, META_DEBUG_WINDOW_OPS = 1 << 7, META_DEBUG_GEOMETRY =
    1 << 20, META_DEBUG_EDGE_RESISTANCE = 1 << 21
}
MetaStackLayer;
struct _MetaWindow
{
  MetaDisplay *display;
  MetaFrame *frame;
  guint user_has_move_resized:1;
  MetaRectangle user_rect;
  XSizeHints size_hints;
};
void meta_window_get_position (MetaWindow * window, int *x, int *y);
typedef struct _MetaFrameGeometry MetaFrameGeometry;
struct _MetaFrameGeometry
{
};
struct _MetaFrame
{
  MetaWindow *window;
  MetaRectangle rect;
  int child_x;
  int child_y;
};
typedef enum
{
  META_IS_CONFIGURE_REQUEST = 1 << 0, META_DO_GRAVITY_ADJUST =
    1 << 3, META_IS_RESIZE_ACTION = 1 << 4
}
MetaMoveResizeFlags;
adjust_for_gravity (MetaWindow * window, MetaFrameGeometry * fgeom,
		    gboolean coords_assume_border, int gravity,
		    MetaRectangle * rect)
{
  int ref_x, ref_y;
  int child_x, child_y;
  int frame_width, frame_height;
  switch (gravity)
    {
    case 1:
      ref_x = rect->x;
    }
  switch (gravity)
    {
    case 1:
      rect->y = ref_y + child_y;
    case 2:
      rect->x = ref_x - frame_width / 2 + child_x;
      break;
    case 3:
    case 5:
    case 6:
      rect->x = ref_x - frame_width + child_x;
    }
}
meta_window_move_resize_internal (MetaWindow * window,
				  MetaMoveResizeFlags flags,
				  int resize_gravity, int root_x_nw,
				  int root_y_nw, int w, int h)
{
  unsigned int mask;
  MetaFrameGeometry fgeom;
  gboolean need_resize_client = (0);
  gboolean is_configure_request;
  MetaRectangle new_rect;
  MetaRectangle old_rect;
  {
    adjust_for_gravity (window, window->frame ? &fgeom : ((void *) 0),
			is_configure_request, window->size_hints.win_gravity,
			&new_rect);
  }
  meta_window_constrain (window, window->frame ? &fgeom : ((void *) 0), flags,
			 resize_gravity, &old_rect, &new_rect);
  if (mask != 0)
    {
      {
	meta_topic_real (META_DEBUG_GEOMETRY,
			 need_resize_client ? "true" : "false");
      }
    }
  {
    window->user_has_move_resized = (!(0));
    meta_window_get_position (window, &window->user_rect.x,
			      &window->user_rect.y);
  }
}
void
meta_window_get_position (MetaWindow * window, int *x, int *y)
{
  if (window->frame)
    {
      *x = window->frame->rect.x + window->frame->child_x;
      *y = window->frame->rect.y + window->frame->child_y;
    }
}
