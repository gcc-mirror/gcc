// PR c++/51415
// { dg-do compile { target c++11 } }

void foo()
{
  int x[1];
  [x]{} = 0;			// { dg-error "lambda" }
}
