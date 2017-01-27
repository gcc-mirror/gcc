
export void Foo (); // { dg-error "after a module" }

module bob;

export
export // { dg-error "occur once" }
void Baz ();

export 
{
  export // { dg-error "occur once" }
    void Bar ();
}

