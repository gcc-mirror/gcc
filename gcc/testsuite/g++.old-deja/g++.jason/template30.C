// { dg-do assemble  }
template <class T, class U>    
int func(U, T);			// { dg-error "" } ref below

template <class T, class U>    
int func(T, U)
{				// { dg-error "" } ref below
        return 2;
}

int main ()
{
  func (0, 1);			// { dg-error "" } ambiguous
}
