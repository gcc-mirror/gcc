/* { dg-do compile } */

typedef struct { char Vshow; } TScreen;
typedef struct _Misc { char Tshow; } Misc;
typedef struct _XtermWidgetRec { TScreen screen; Misc misc; } XtermWidgetRec, *XtermWidget;
extern XtermWidget term;
void Bell (XtermWidget, int, int);
void set_tek_visibility (int);

void
handle_tekshow (void *gw, int allowswitch)
{
  XtermWidget xw = term;
  if (!((xw)->misc.Tshow))
    set_tek_visibility (1);
}

void
do_tekonoff (void *gw, void *closure, void *data)
{
  handle_tekshow (gw, 0);
}

void
do_vtonoff (void *gw, void *closure, void *data)
{
}

void
handle_toggle (void (*proc) (void *gw, void *closure, void *data),
	       int var, char **params, unsigned int nparams, void *w,
	       void *closure, void *data)
{
  XtermWidget xw = term;
  int dir = -2;
  switch (nparams)
    {
    case 0:
      dir = -1;
    }
  switch (dir)
    {
    case 1:
      (*proc) (w, closure, data);
      Bell (xw, 2, 0);
    }
}

void
HandleVisibility (void *w, char **params, unsigned int *param_count)
{
  XtermWidget xw = term;
  if (*param_count == 2)
    switch (params[0][0])
      {
      case 'v':
	handle_toggle (do_vtonoff, (int) ((int) (&(xw)->screen)->Vshow),
		       params + 1, (*param_count) - 1, w, (void *) 0,
		       (void *) 0);
	handle_toggle (do_tekonoff, (int) ((int) ((xw)->misc.Tshow)),
		       params + 1, (*param_count) - 1, w, (void *) 0,
		       (void *) 0);
      }
}
