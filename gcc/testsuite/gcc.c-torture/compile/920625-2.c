typedef	char *	caddr_t;
typedef unsigned Cursor;
typedef char *String;
typedef struct _WidgetRec *Widget;
typedef char		Boolean;
typedef unsigned int	Cardinal;
typedef struct _XedwListReturnStruct {
  String   string;
  int      xedwList_index;
  struct _XedwListReturnStruct *next;
} XedwListReturnStruct;
static XedwListReturnStruct *return_list;
static String   srcdir, dstdir;
char *strcpy();
  extern void setCursor(Cursor);
  extern void query_dialog(String, Boolean);
  extern Boolean directoryManagerNewDirectory(String);
trashQueryResult(Widget w, Boolean delete, caddr_t call_data)
{
  int  execute(String, String, String, Boolean);
  extern void destroy_button_dialog(void);
  extern void changestate(Boolean);

  extern Cursor busy, left_ptr;
  extern String cwd;
      	static void freeReturnStruct(void);
  String rmstring;
  int status;
  XedwListReturnStruct *tmp;
  setCursor(busy);
  destroy_button_dialog();
  if (delete == 1) {
    rmstring = (("rm -fr") != ((void *)0) ? (strcpy((char*)XtMalloc((unsigned)strlen("rm -fr") + 1), "rm -fr")) : ((void *)0));
    tmp = return_list;
    while (tmp != ((void *)0)) {
      rmstring = (String) XtRealloc (rmstring, sizeof(char) *
				     (strlen(rmstring) +
				      strlen(tmp->string) + 5));
      sprintf(rmstring, "%s '%s'", rmstring, tmp->string);
      tmp = tmp->next;
    }
    if ((status = execute(((void *)0), "rm", rmstring, 1)) != 0) {
      XBell(XtDisplay(w), 100);
      query_dialog("Can't remove file", 0);
    }
    XtFree(rmstring);

    directoryManagerNewDirectory(cwd);
  } else {
    changestate(1);
  }
  setCursor(left_ptr);
  freeReturnStruct();
}

copyQueryResult(Widget w, Boolean copy, caddr_t call_data)
{
  extern void destroy_button_dialog();
  extern void changestate(Boolean);
  extern Cursor busy, left_ptr;
  static void freeReturnStruct(void);
  int execute(String, String, String, Boolean);
  extern String cwd;
  String copystring;
  int status;
  Cardinal srclen, dstlen;
  XedwListReturnStruct *tmp;
  destroy_button_dialog();
  setCursor(busy);
  if (copy == 1) {
    srclen = strlen(srcdir);
    dstlen = strlen(dstdir);
    copystring = (("cp -r") != ((void *)0) ? (strcpy((char*)XtMalloc((unsigned)strlen("cp -r") + 1), "cp -r")) : ((void *)0));
    tmp = return_list;
    while (tmp != ((void *)0)) {
      copystring = (String) XtRealloc (copystring, sizeof(char) *
				       (strlen(copystring) +
					strlen(tmp->string) +
					srclen + 6));
      sprintf(copystring, "%s '%s/%s'", copystring, srcdir, tmp->string);
      tmp = tmp->next;
    }
    copystring = (String) XtRealloc (copystring, sizeof(char) *
				     (strlen(copystring) +
				      dstlen + 5));
    sprintf(copystring, "%s '%s'", copystring, dstdir);
    if ((status = execute(((void *)0), "cp", copystring, 1)) != 0) {
      XBell(XtDisplay(w), 100);
      query_dialog("Can't copy file!", 0);
    }
    XtFree(copystring);

    directoryManagerNewDirectory(cwd);
  } else {
    changestate(1);
  }
  XtFree(srcdir);
  XtFree(dstdir);
  setCursor(left_ptr);
  freeReturnStruct();
}

freeReturnStruct(){}
