// PR c++/115469
// { dg-do compile { target indirect_jumps } }
// { dg-options "" }

void
fn1 ()
{
  b = &&c;    // { dg-error "not declared|not defined" }
  goto *0;
}

void
fn2 ()
{
c:
  b = &&c;  // { dg-error "not declared" }
  goto *0;
}
