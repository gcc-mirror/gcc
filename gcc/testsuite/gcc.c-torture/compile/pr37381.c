/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */
/* { dg-additional-options "-fpermissive" } */

extern unsigned int __invalid_size_argument_for_IOC;
typedef unsigned int __u32;
struct video_window
{
  __u32 x, y;
  __u32 width, height;
};
typedef unsigned long XID;
typedef XID Window;
typedef struct _XExtData
{
  Window root;
}
Screen;
typedef struct
{
  int border_width;
}
XWindowAttributes;
typedef struct _XDisplay Display;
typedef struct
{
  int default_screen;
  Screen *screens;
}
 *_XPrivDisplay;
typedef struct
{
  int x, y;
}
XSizeHints;
typedef struct
{
  unsigned short hdisplay;
  unsigned short vdisplay;
}
XF86VidModeModeInfo;
Display *display;
int tfd;
int ccapt;
int tml;
int fswidth = 0;
int fsheight = 0;
Window fmwin;
XF86VidModeModeInfo **modelines, *fullscreenmode = ((void *) 0);
struct video_window vswin;
DoFullScreen (void)
{
  int i;
  int rx, ry;
  Window junkwin;
  XSizeHints fmsizehints;
  XWindowAttributes fmwinattr;
  if (ioctl
      (tfd,
       (((1U) << (((0 + 8) + 8) + 14)) | ((('v')) << (0 + 8)) | (((8)) << 0) |
	(((((sizeof (int) == sizeof (int[1])
	     && sizeof (int) <
	     (1 << 14)) ? sizeof (int) : __invalid_size_argument_for_IOC))) <<
	 ((0 + 8) + 8))), &ccapt) < 0)
    {
      perror ("ioctl VIDIOCCAPTURE");
    }
  if (!XTranslateCoordinates
      (display, fmwin,
       ((&((_XPrivDisplay) display)->
	 screens[(((_XPrivDisplay) display)->default_screen)])->root),
       -fmwinattr.border_width, -fmwinattr.border_width, &rx, &ry, &junkwin))
    {
    }
  vswin.width = fswidth;
  vswin.height = fsheight;
  vswin.x = fmsizehints.x + rx;
  vswin.y = fmsizehints.y + ry;
  if (ioctl
      (tfd,
       (((1U) << (((0 + 8) + 8) + 14)) | ((('v')) << (0 + 8)) | (((8)) << 0) |
	(((((sizeof (int) == sizeof (int[1])
	     && sizeof (int) <
	     (1 << 14)) ? sizeof (int) : __invalid_size_argument_for_IOC))) <<
	 ((0 + 8) + 8))), &ccapt) < 0)
    {
      XF86VidModeGetAllModeLines (display, XDefaultScreen (display), &tml,
				  &modelines);
	{
	  if ((modelines[i]->hdisplay == fswidth)
	      && (modelines[i]->vdisplay == fsheight))
	    {
	      fullscreenmode = modelines[i];
	    }
	}
	{
	  XF86VidModeSetViewPort (display, XDefaultScreen (display), vswin.x,
				  vswin.y);
	}
    }
}
