/* { dg-do compile } */

struct rgba8;
template<class ColorT> class span_gouraud     {
public:
    struct coord_type { };
    coord_type m_coord[3];
};
template<class ColorT> class span_gouraud_rgba : public span_gouraud<ColorT>   
{
  typedef ColorT color_type;
  typedef span_gouraud<color_type> base_type;
  typedef typename base_type::coord_type coord_type;
public:
  void prepare()         {
      coord_type coord[3];
  }
};
void the_application() {
    typedef span_gouraud_rgba<rgba8> gouraud_span_gen_type;
    gouraud_span_gen_type span_gouraud;
    span_gouraud.prepare();
}
