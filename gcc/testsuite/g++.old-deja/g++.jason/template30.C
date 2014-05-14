// { dg-do assemble  }
template <class T, class U>    
int func(U, T);			// { dg-message "note" }

template <class T, class U>    
int func(T, U)			// { dg-message "note" }
{
        return 2;
}

int main ()
{
  func (0, 1);			// { dg-error "ambiguous" }
}
