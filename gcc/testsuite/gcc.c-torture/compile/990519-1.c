  typedef int   gboolean;

  typedef struct{
    gboolean names : 1;
    gboolean types : 1;
  } ParamOptions;

  int p_param(ParamOptions* o){
    return o->types && o->names;
  }
