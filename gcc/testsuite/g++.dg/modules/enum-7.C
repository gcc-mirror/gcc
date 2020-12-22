// { dg-additional-options -fmodules-ts }

// ICE getting template info of a function-scope enum

template <typename T> void adl (T) {}

template <typename T>
void frob (T arg)
{
  enum class case_conv {none};

  case_conv x = case_conv::none;

  adl (x);
}

void foo ()
{
  frob (1);
}
