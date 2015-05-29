/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-inline -fdump-ipa-cp -fno-early-inlining" } */
class wxPaintEvent {  };
struct wxDCBase
{ 
  wxDCBase ();
  virtual int GetLayoutDirection() const{}
  virtual void SetLayoutDirection(int){}
};
struct wxWindowDC  : public wxDCBase {};
struct wxBufferedDC  : public wxDCBase
{ 
  void Init(wxDCBase*dc) {
    InitCommon(dc);
  }
  void InitCommon(wxDCBase*dc) {
    if (dc)
      SetLayoutDirection(dc->GetLayoutDirection());
  }
};
struct wxBufferedPaintDC  : public wxBufferedDC {
  wxBufferedPaintDC() {
    Init(&m_paintdc);
  }
 wxWindowDC m_paintdc;
};
void  OnPaint(wxPaintEvent & event) {
  wxBufferedPaintDC dc;
}
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 2 "cp"  } } */
