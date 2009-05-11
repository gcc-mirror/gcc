extern void *ff(void*,int);

struct lpgl { struct lpgl *next; };
struct lpgd { struct lpgl *first; };

typedef int (*xfn)( );
static void xDP_IF_EnumGroupsInGroup ( void *a, int b, xfn fn)
{
  struct lpgd *lpGData;
  struct lpgl *lpGList;

  if( ( lpGData = ff( a, b ) ) == ((void *)0) )
    return;

  if( lpGData->first  == ((void *)0) )
    return;
  lpGList = lpGData->first;

  for( ;; ) {
    if( !(*fn)( ) )
      return;
    if( lpGList->next == ((void *)0) )
      break;
    lpGList = lpGList->next;
  }
  return;
}


static int 
xcbDeletePlayerFromAllGroups() {
  xDP_IF_EnumGroupsInGroup(0, 0, 0);
  return 1;
}

void xDP_IF_EnumGroups( xfn fn) {
  xDP_IF_EnumGroupsInGroup( 0, 0, fn);
}

static void xDP_IF_DestroyPlayer () {
  xDP_IF_EnumGroups( xcbDeletePlayerFromAllGroups);
}

void* foo=xDP_IF_DestroyPlayer;
