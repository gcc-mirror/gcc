// { dg-lto-do link }
// { dg-lto-options { { -flto -O2 } } }
// { dg-extra-ld-options "-r -nostdlib -O0" }

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
class ZEvent_Component { };
class ZEvent_Data { };
class ZEvent_Interrupt { };
class ZEvent_Mouse { };
class ZEvent_Key { };
class ZEventHandler {
  virtual void HandleEvent (const ZEvent_Component & event);
  virtual void HandleEvent (const ZEvent_Mouse & event);
  virtual void HandleEvent (const ZEvent_Key & event);
  virtual void HandleEvent (const ZEvent_Interrupt & event);
  virtual void HandleEvent (const ZEvent_Data & event);
};
enum ZHorizontalAlignment { HA_Left };
enum ZVerticalAlignment { VA_Baseline };
struct ZDevicePointStruct { };
struct ZDeviceRectangleStruct { };
struct ZDeviceCircleStruct { };
class ZOutputDevice;
class Foo;
class ZRubberBand {
 public:
  ZOutputDevice * getOutputDevice ();
};
class ZRubberBand2P : public ZRubberBand {
  virtual void Init ();
  Foo *mp_graphicContext;
  int m_textAscent;
  int m_OkButtonWidth;
  int m_OkButtonHeight;
};
class ZColor { };
class ZViewPort2D { };
class ZCursor;
class ZPixmap;
class ZOutputDevice:public ZEventHandler {
public:
  typedef ZHorizontalAlignment THorizontalAlignment;
  typedef ZVerticalAlignment TVerticalAlignment;
  virtual const char *MyName () const { return ""; }
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
  virtual ZCursor *createCursor (const ZPixmap &, int, int) { return __null; }
  virtual void DrawLine (const Foo & gc, int x1, int y1, int x2, int y2) = 0;
  virtual void DrawLines (const Foo & gc, const ZDevicePointStruct * points, unsigned int count) = 0;
  virtual void FillPolygon (const Foo & gc, const ZDevicePointStruct * points, unsigned int count) = 0;
  virtual void DrawPoint (const Foo & gc, int x1, int y1) = 0;
  virtual void DrawPoints (const Foo & gc, const ZDevicePointStruct * points, unsigned int count) = 0;
  virtual void DrawRectangle (const Foo & gc, int x, int y, int width, int height) = 0;
  virtual void DrawRectangles (const Foo & gc, const ZDeviceRectangleStruct * rectangles, unsigned int count) = 0;
  virtual void FillRectangle (const Foo & gc, int x, int y, int width, int height) = 0;
  virtual void FillRectangles (const Foo & gc, const ZDeviceRectangleStruct * rectangles, unsigned int count) = 0;
  virtual void DrawCircle (const Foo & gc, int x, int y, int radius) = 0;
  virtual void DrawCircles (const Foo & gc, const ZDeviceCircleStruct * circle, unsigned int count) = 0;
  virtual void FillCircle (const Foo & gc, int x, int y, int radius) = 0;
  virtual void FillCircles (const Foo & gc, const ZDeviceCircleStruct * circle, unsigned int count) = 0;
  virtual void DrawString (const Foo & gc, int xx, int yy, const Cstring & theString, THorizontalAlignment horAlign = HA_Left, TVerticalAlignment verAlign = VA_Baseline) = 0;
  virtual void getStringBounds (const Foo & gc, const Cstring & theString, int & width, int & height, int & ascent) const;
};
template < class T > class EMaskContentVector
{
  signed m_freelist_idx:32;
  EMaskContentVector (const EMaskContentVector < T > &, void *buf);
};
template < class T > EMaskContentVector <
  T >::EMaskContentVector (const EMaskContentVector < T > &elem, void *buf):
m_freelist_idx (-1)
{
}
void ZRubberBand2P::Init () {
  getOutputDevice ()->getStringBounds (*mp_graphicContext, Cstring ("Ok"), m_OkButtonWidth, m_OkButtonHeight, m_textAscent);
}
