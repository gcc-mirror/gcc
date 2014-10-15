/* PR opt/6516 */
/* On x86, something about the structure of this function prevented
   cross-jumping from combining the three arms of the switch statement
   until the last moment.  After which we did not delete dead code, 
   which left a reference to the deleted ADDR_VEC.  */

/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -frename-registers -fpic" } */

typedef unsigned long XID;
typedef XID Window;
typedef void Display;
typedef unsigned long Time;

typedef struct {
        int type;
        unsigned long serial;
        int send_event;
        Display *display;
        Window window;
        Window root;
        Window subwindow;
        Time time;
        int x, y;
        int x_root, y_root;
        unsigned int state;
        unsigned int button;
        int same_screen;
} XButtonEvent;
typedef struct {
        int type;
        unsigned long serial;
        int send_event;
        Display *display;
        Window window;
        Window root;
        Window subwindow;
        Time time;
        int x, y;
        int x_root, y_root;
        unsigned int state;
        char is_hint;
        int same_screen;
} XMotionEvent;
typedef struct {
        int type;
        unsigned long serial;
        int send_event;
        Display *display;
        Window window;
        Window root;
        Window subwindow;
        Time time;
        int x, y;
        int x_root, y_root;
        int mode;
        int detail;
        int same_screen;
        int focus;
        unsigned int state;
} XCrossingEvent;
typedef union _XEvent {
        int type;
        XButtonEvent xbutton;
        XMotionEvent xmotion;
        XCrossingEvent xcrossing;
} XEvent;

typedef struct {
        int width, height;
        Display *display;
} Screen;
typedef struct _CorePart {
    Screen *screen;
} CorePart;
typedef struct _WidgetRec {
    CorePart core;
} WidgetRec;
typedef struct _WidgetRec *Widget;

typedef struct _SmeRec *SmeObject;
typedef struct _SimpleMenuPart {
    SmeObject entry_set;
} SimpleMenuPart;
typedef struct _SimpleMenuRec {
    SimpleMenuPart simple_menu;
} SimpleMenuRec;
typedef struct _SimpleMenuRec* SimpleMenuWidget;

typedef short Position;
typedef unsigned short Dimension;
typedef char Boolean;

typedef struct _RectObjPart {
    Position x, y;
    Dimension width, height;
    Dimension border_width;
    Boolean managed;
    Boolean sensitive;
    Boolean ancestor_sensitive;
}RectObjPart;
typedef struct _RectObjRec {
    RectObjPart rectangle;
} RectObjRec;
typedef struct _RectObjRec *RectObj;

SmeObject DoGetEventEntry();
int XtWidgetToApplicationContext();
void XtAppError();
void Unhighlight();
void XtMoveWidget();
void XWarpPointer();

SmeObject
GetEventEntry(Widget w, XEvent *event)
{
    int x_loc, y_loc, x_root;
    SimpleMenuWidget smw = (SimpleMenuWidget)w;
    SmeObject entry;
    int warp, move;
    switch (event->type) {
        case 6:
            x_loc = event->xmotion.x;
            y_loc = event->xmotion.y;
            x_root = event->xmotion.x_root;
            break;
        case 7:
        case 8:
            x_loc = event->xcrossing.x;
            y_loc = event->xcrossing.y;
            x_root = event->xcrossing.x_root;
            break;
        case 4:
        case 5:
            x_loc = event->xbutton.x;
            y_loc = event->xbutton.y;
            x_root = event->xbutton.x_root;
            break;
        default:
            XtAppError(XtWidgetToApplicationContext(w),
                       "Unknown event type in GetEventEntry().");
            return (((void *)0));
    }
    if (x_loc < 0 || x_loc >= (((RectObj)smw)->rectangle.width) ||
        y_loc < 0 || y_loc >= (((RectObj)smw)->rectangle.height))
        return (((void *)0));
    if (x_root == ((((w)->core.screen))->width) - 1 &&
        (((RectObj)w)->rectangle.x) + (((RectObj)w)->rectangle.width) + ((((RectObj)w)->rectangle.border_width)) > x_root) {
        warp = -8;
        if (smw->simple_menu.entry_set) {
            entry = DoGetEventEntry(w,
                                    (((RectObj)smw->simple_menu.entry_set)->rectangle.x)
                                    + (((RectObj)smw->simple_menu.entry_set)->rectangle.width) + 1,
                                    y_loc);
            Unhighlight(w, event, ((void *)0), ((void *)0));
            if (entry) {
                warp = -(int)(((RectObj)entry)->rectangle.width) >> 1;
                move = x_loc - (((RectObj)entry)->rectangle.width) - (((RectObj)entry)->rectangle.x) + (((RectObj)w)->rectangle.border_width);
            }
            else {
                warp = 0;
                move = ((((w)->core.screen))->width) -
                       ((((RectObj)w)->rectangle.x) + (((RectObj)w)->rectangle.width) + ((((RectObj)w)->rectangle.border_width) << 1));
            }
        }
        else {
            warp = 0;
            move = ((((w)->core.screen))->width) -
                   ((((RectObj)w)->rectangle.x) + (((RectObj)w)->rectangle.width) + ((((RectObj)w)->rectangle.border_width) << 1));
        }
    }
    else if (x_root == 0 && (((RectObj)w)->rectangle.x) < 0) {
        warp = 8;
        if (smw->simple_menu.entry_set) {
            entry = DoGetEventEntry(w, (((RectObj)smw->simple_menu.entry_set)->rectangle.x) - 1,
                                    y_loc);
            Unhighlight(w, event, ((void *)0), ((void *)0));
            if (entry) {
                warp = (((RectObj)entry)->rectangle.width) >> 1;
                move = x_loc - (((RectObj)entry)->rectangle.x);
            }
            else
                move = x_loc + (((RectObj)w)->rectangle.border_width);
        }
        else
            move = x_loc + (((RectObj)w)->rectangle.border_width);
    }
    else
        move = warp = 0;
    if (move)
        XtMoveWidget(w, (((RectObj)w)->rectangle.x) + move, (((RectObj)w)->rectangle.y));
    if (warp)
        XWarpPointer((((w)->core.screen)->display), 0L, 0L, 0, 0, 0, 0, warp, 0);
    return (DoGetEventEntry(w, x_loc, y_loc));
}
