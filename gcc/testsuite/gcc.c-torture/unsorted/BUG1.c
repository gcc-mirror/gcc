struct _XtTextSource {
	/* ... */
    	void (* SetSelection)();
	/* ... */
    };

typedef struct _XtTextSource *XtTextSource;

typedef struct _TextPart {
    	XtTextSource source;
	/* ... */
} TextPart;

typedef struct _TextRec {
	/* ... */
	TextPart    text;
} TextRec;

typedef struct _TextRec      *TextWidget;


void XtTextUnsetSelection(w)
    TextWidget          w;		   /* original is: Widget w; */
{
    register TextWidget ctx = (TextWidget) w;
    void (*nullProc)() = 0;

/*
 * the following line causes the error, when optimizing:
 */

    if (ctx->text.source->SetSelection != nullProc) {

	foo();

    }
}
