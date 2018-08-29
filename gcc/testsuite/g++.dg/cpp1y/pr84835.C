// { dg-do compile { target c++14 } }
// PR c++/84835
// ICE with generic lambda inside extern "C"

extern "C" 
{
  auto r = [] (auto x) 
  {
    void baz (); // extern "C"
    baz ();
  };
  
}

void g ()
{
  r (0);
}

//  { dg-final { scan-assembler "\[^0-9\]baz" } }
