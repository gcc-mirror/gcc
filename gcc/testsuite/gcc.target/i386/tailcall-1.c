/* { dg-do compile } */
/* { dg-skip-if "" { { i?86-*-* x86_64-*-* } && ilp32 } { "-fpic" "-fPIC" } { "" } } */
/* { dg-options "-O2" } */

typedef unsigned int Cardinal;
typedef char *String;
typedef struct _WidgetRec *Widget;

typedef union _XEvent {
        int type;
 long pad[24];
} XEvent;


extern int SendMousePosition (Widget w, XEvent* event);


void
HandleIgnore(Widget w,
      XEvent * event,
      String * params ,
      Cardinal *param_count )
{

    (void) SendMousePosition(w, event);
}

/* { dg-final { scan-assembler "jmp" } } */
