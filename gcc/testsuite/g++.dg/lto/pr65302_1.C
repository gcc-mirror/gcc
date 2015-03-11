#pragma implementation
#pragma interface
class CstringStorageReference {
  public:
  ~CstringStorageReference ();
};
class Cstring {
  CstringStorageReference m_stringRef;
 public:
  Cstring (const char *str, int l = 0);
  unsigned int getLength () const;
};
inline unsigned int
Cstring::getLength () const { };
class ZEvent_Component { };
class ZEvent_Data { };
class ZEvent_Interrupt { };
class ZEvent_Mouse { };
class ZEvent_Key { };
class ZEventHandler
{
  virtual void HandleEvent (const ZEvent_Component & event);
  virtual void HandleEvent (const ZEvent_Mouse & event);
  virtual void HandleEvent (const ZEvent_Key & event);
  virtual void HandleEvent (const ZEvent_Interrupt & event);
  virtual void HandleEvent (const ZEvent_Data & event);
};
class ZColor { };
class ZViewPort2D { };
enum ZVerticalAlignment { VA_Baseline };
struct ZDevicePointStruct { };
class ZCursor;
class ZPixmap;
class Foo;
class ZOutputDevice : public ZEventHandler {
 public:
  typedef ZVerticalAlignment TVerticalAlignment;
  virtual const char *MyName () const { }
  virtual ~ ZOutputDevice ();
  virtual Cstring getTitle () const;
  virtual void setTitle (const Cstring &) { }
  virtual void Init ();
  virtual void shutdown ();
  virtual void minimize ();
  virtual void normalize ();
  virtual void raiseToTop ();
  virtual ZViewPort2D GetViewPort () const;
  virtual void setBackgroundColor (const ZColor & color) = 0;
  virtual void Clear () = 0;
  virtual void Flush (int forced) = 0;
  virtual void dismissCache () { }
  virtual int GetDeviceWidth () const = 0;
  virtual int GetDeviceHeight () const = 0;
  virtual Foo *CreateGraphicContext () = 0;
  virtual ZCursor *createCursor (const ZPixmap &, int, int) { }
  virtual void DrawLine (const Foo & gc, int x1, int y2) = 0;
  virtual void DrawLines (const Foo & gc, const ZDevicePointStruct * points,
			  unsigned int count) = 0;
};
class ZOutputDevicePS :public ZOutputDevice
{
  virtual void FillPolygon (const Foo & gc, unsigned int count);
  virtual void DrawPoint (const Foo & gc, int x1, int y1);
  virtual void DrawPoints (const Foo & gc, const ZDevicePointStruct * points,
			   unsigned int count);
  virtual void DrawRectangle (const Foo & gc, int x, int height);
  virtual void DrawRectangles (const Foo & gc, unsigned int count);
  virtual void FillRectangle (const Foo & gc, int x, int height);
  virtual void FillRectangles (const Foo & gc, unsigned int count);
  virtual void DrawCircle (const Foo & gc, int x, int y, int radius);
  virtual void DrawCircles (const Foo & gc, unsigned int count);
  virtual void FillCircle (const Foo & gc, int x, int y, int radius);
  virtual void FillCircles (const Foo & gc, unsigned int count);
  virtual void DrawString (const Foo & gc, int xx, int yy,
			   TVerticalAlignment verAlign);
  virtual void getStringBounds (const Foo & gc, const Cstring & theString,
				int & width, int & height, int & acsent) const;
};
void
ZOutputDevicePS::getStringBounds (const Foo &, const Cstring & theString,
				   int & width, int & height, int & ascent) const {
  width = theString.getLength () * 8;
}
