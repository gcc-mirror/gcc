/* { dg-do compile } */
int  ggggg();
struct string {
  static int _S_empty_rep_storage[];
  void  _M_destroy();
  char* _M_rep;
  ~string()
  {
    if (_M_rep != (char*)&_S_empty_rep_storage )
      if (ggggg() <= 0)
        _M_destroy();
  }
};
extern void SDL_FreeSurface(int surface);
struct scoped_resource {
  ~scoped_resource()  {
    SDL_FreeSurface(1);
  }
};
struct surface {
  scoped_resource surface_;
};
struct button {
  string help_text_;
  string label_;
  surface image_;
};
struct scrollbar {
  string help_text_;
  button uparrow_;
};
scrollbar a;

