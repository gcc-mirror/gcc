/* { dg-do compile } */
typedef struct {
    unsigned long attr;
    int chars[2];
} cchar_t;
typedef struct _win_st {
    cchar_t _bkgrnd;
} WINDOW;
void render_char(WINDOW *win, cchar_t ch)
{
    if ((ch).chars[0] == L' '
        && (ch).chars[1] == L'\0')
        win->_bkgrnd = ch;
}
