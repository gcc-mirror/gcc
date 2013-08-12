// PR c++/57416
// { dg-do compile { target c++11 } }

struct Nothing
{
};

template <class PARENTDATA>
void func3 (PARENTDATA & p_parent_data)
{
  struct Data
  {
    PARENTDATA & parent_data = p_parent_data;  // { dg-error "parameter" }
  } data;
}

template <class PARENTDATA>
void func2 (PARENTDATA & p_parent_data)
{
  struct Data
  {
    PARENTDATA & parent_data = p_parent_data;  // { dg-error "parameter" }
  } data;

  data.parent_data.x = 5;
  func3(data);
}

template <class PARENTDATA>
void func1 (PARENTDATA & p_parent_data)
{
  struct Data
  {
    PARENTDATA & parent_data = p_parent_data;  // { dg-error "parameter" }
    int x = 1;
  } data;

  func2(data);
}

int main()
{
  Nothing nothing;
  func1(nothing);
}
