/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef __SIZE_TYPE__ size_t;
typedef void *XtPointer;

typedef struct _WidgetRec *Widget;
typedef struct _WidgetClassRec *WidgetClass;

extern WidgetClass commandWidgetClass;

typedef void (*XtCallbackProc)(
    Widget 		 ,
    XtPointer 		 ,	 
    XtPointer 		 	 
);

extern const  char XtStrings[];

extern Widget XtVaCreateManagedWidget(const char *, WidgetClass, Widget, ...);
extern void XtAddCallback(const char *, XtCallbackProc, XtPointer);

typedef struct						 
{
	char			*Name,			 
				*Label;			 
	XtCallbackProc		Callback;		 
	XtPointer		ClientData;		 
	Widget			W;				 
} DialogButtonType, *DialogButtonTypePtr;

 
Widget AddButtons(Widget Parent, Widget Top,
	DialogButtonTypePtr Buttons, size_t Count)
{
	int		i;

	for (i = 0; i < Count; i++)
	{
		if (!Buttons[i].Label)
			continue;
		Buttons[i].W  = XtVaCreateManagedWidget(Buttons[i].Name,
			commandWidgetClass,
			Parent,
			((char*)&XtStrings[429]) , Buttons[i].Label,
			"fromHoriz" , i ? Buttons[i-1].W : ((void *)0) ,
			"fromVert" , Top,
			"resizable" , 1 ,
			((void *)0) );

		XtAddCallback(((char*)&XtStrings[136]),
				 Buttons[i].Callback, Buttons[i].ClientData);
	}
	return(Buttons[Count-1].W);
}

