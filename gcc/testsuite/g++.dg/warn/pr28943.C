// PR c++/28943 void and non-void in conditional expression
// { dg-do compile }
// { dg-options "" }

void debug (const char * string)
{
  return;
}

int f()
{
  ( true == false ? 0 : debug ("Some string")); // { dg-error "third operand .* type 'void'.* second operand is neither a throw-expression nor of type 'void'" }
  ( true == false ? debug ("Some string") : 0 ); // { dg-error "second operand .* type 'void'.* third operand is neither a throw-expression nor of type 'void'" }
  return 0;
}
