template <class T, class U>    
int func(U, T);			// ERROR - ref below

template <class T, class U>    
int func(T, U)
{				// ERROR - ref below
        return 2;
}

int main ()
{
  func (0, 1);			// ERROR - ambiguous
}
