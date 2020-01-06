struct mouse_button_str {
        unsigned char left      : 1;
        unsigned char right     : 1;
        unsigned char middle    : 1;
};
int g(void)
{
  unsigned char a = 0;
  struct mouse_button_str *newbutton1 = (struct mouse_button_str*)&a;
  newbutton1->left = 1;
  return a;
}
