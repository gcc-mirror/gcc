// PR sanitizer/86406
// { dg-do compile }
// { dg-options "-fcompare-debug -fsanitize=undefined -g -O1" }

typedef enum { } cmd_status;
class ECell;
class ECell_const_ptr { };
class ECell_ptr
{
  ECell *mp_element;
  ECell *getPointer () const { return mp_element; }
public:
  operator  ECell_const_ptr () const { return ECell_const_ptr(); }
};

extern ECell_ptr NULL_CELL;
class VwUI_2DCellLayerView;
class view_cell_layoutImpl
{
  cmd_status handleChangeFlags (VwUI_2DCellLayerView *
                                      p_ui_celllayerview,
                                      ECell_const_ptr p_peekCell);
  cmd_status openCellLayoutView ();
};

cmd_status
view_cell_layoutImpl::openCellLayoutView ()
{
  ECell_const_ptr pcell = NULL_CELL;
  VwUI_2DCellLayerView *p_user_interface;
  return handleChangeFlags (p_user_interface, pcell);
  ;
}
