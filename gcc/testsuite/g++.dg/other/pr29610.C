/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops" } */

struct __normal_iterator 
{
  typedef int*const *_Iterator;
  int*const * _M_current;
  __normal_iterator(const _Iterator& __i) : _M_current(__i){}
  const _Iterator& base() const { static _Iterator a; return a; }
};
struct string { ~string(){} };
struct vector 
{
  int** _M_finish;
  __normal_iterator end() const   { return __normal_iterator (_M_finish); }
  int size() const   { return end().base() - end().base(); }
};
class Painter
{
  int redraw_window(void);
  typedef int (Painter::* SliceWindowFunc)(void);
  int for_each(vector&, SliceWindowFunc);
  void tcl_command(void);
};
inline int Painter::for_each(vector &layout, SliceWindowFunc func)
{
    for (unsigned int window = 0; window < layout.size();++window)
        (this->*func)();
    return 0;
}
int t;
int Painter::redraw_window(void) {t = 1; return 0; }
string t2(int);
vector *g(const string&);
void Painter::tcl_command(void)
{
     for_each(*g(t2(2)), &Painter::redraw_window);
}

