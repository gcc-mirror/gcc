struct WView
{
  int hexedit_mode:1;
};
void
toggle_hexedit_mode (struct WView *view)
{
  if (view->hexedit_mode)
    {
    }
  else
    {
      view->hexedit_mode = !view->hexedit_mode;
    }
}
