
export void Foo (); // { dg-error "after a module" }

module bob;

export
export // { dg-error "appear once" }
void Baz ();

export 
{
  export // { dg-error "appear once" }
    void Bar ();
}

