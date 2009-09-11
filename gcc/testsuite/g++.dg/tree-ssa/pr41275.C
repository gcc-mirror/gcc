// PR middle-end/41275
// { dg-do compile }
// { dg-options "-O2" }
// this used to ICE
struct ErrmsgWindow
{
    virtual ~ErrmsgWindow()  
    {
      extern int _switch_mode_errorstr;
      _switch_mode_errorstr = 42;
    }
};
void ShowErrorMessage(void)
{
  ErrmsgWindow w;
}
