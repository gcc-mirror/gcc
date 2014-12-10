// PR c++/20721

bool f();
void g(int i)
{
  if (i) goto bad; // { dg-message "from" }
  bool a = f(); // { dg-message "initialization" }
 bad: // { dg-error "jump" }
  ;
}

