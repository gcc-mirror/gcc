/* Function definitions that are used by multiple tests.  */

void initSfd (Sfd *p, double y)
{ p->f = y; p->d = y+1; }
void initSfl (Sfl *p, double y)
{ p->f = y; p->l = y+1; }
void initSdf (Sdf *p, double y)
{ p->d = y; p->f = y+1; }
void initSdl (Sdl *p, double y)
{ p->d = y; p->l = y+1; }
void initSlf (Slf *p, double y)
{ p->l = y; p->f = y+1; }
void initSld (Sld *p, double y)
{ p->l = y; p->d = y+1; }

void initSfdl (Sfdl *p, double y)
{ p->f = y; p->d = y+1; p->l = y+2; }
void initSfld (Sfld *p, double y)
{ p->f = y; p->l = y+1; p->d = y+2; }
void initSdfl (Sdfl *p, double y)
{ p->d = y; p->f = y+1; p->l = y+2; }
void initSdlf (Sdlf *p, double y)
{ p->d = y; p->l = y+1; p->f = y+2; }
void initSlfd (Slfd *p, double y)
{ p->l = y; p->f = y+1; p->d = y+2; }
void initSldf (Sldf *p, double y)
{ p->l = y; p->d = y+1; p->f = y+2; }
