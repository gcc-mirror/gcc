/* Function definitions that are used by multiple tests.  */

void checkSfd (Sfd x, double y)
{ if (x.f != y || x.d != y+1) DEBUG_CHECK }
void checkSfl (Sfl x, double y)
{ if (x.f != y || x.l != y+1) DEBUG_CHECK }
void checkSdf (Sdf x, double y)
{ if (x.d != y || x.f != y+1) DEBUG_CHECK }
void checkSdl (Sdl x, double y)
{ if (x.d != y || x.l != y+1) DEBUG_CHECK }
void checkSlf (Slf x, double y)
{ if (x.l != y || x.f != y+1) DEBUG_CHECK }
void checkSld (Sld x, double y)
{ if (x.l != y || x.d != y+1) DEBUG_CHECK }

void checkSfdl (Sfdl x, double y)
{ if (x.f != y || x.d != y+1 || x.l != y+2) DEBUG_CHECK }
void checkSfld (Sfld x, double y)
{ if (x.f != y || x.l != y+1 || x.d != y+2) DEBUG_CHECK }
void checkSdfl (Sdfl x, double y)
{ if (x.d != y || x.f != y+1 || x.l != y+2) DEBUG_CHECK }
void checkSdlf (Sdlf x, double y)
{ if (x.d != y || x.l != y+1 || x.f != y+2) DEBUG_CHECK }
void checkSlfd (Slfd x, double y)
{ if (x.l != y || x.f != y+1 || x.d != y+2) DEBUG_CHECK }
void checkSldf (Sldf x, double y)
{ if (x.l != y || x.d != y+1 || x.f != y+2) DEBUG_CHECK }
