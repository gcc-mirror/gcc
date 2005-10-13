// PR c++/20721

bool f();
void g(int i)
{
  if (i) goto bad; // { dg-error "from" }
  bool a = f(); // { dg-error "initialization" }
 bad: // { dg-error "jump" }
  ;
}

