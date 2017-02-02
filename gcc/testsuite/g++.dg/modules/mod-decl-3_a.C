
export void Foo (); // { dg-error "after an interface module" }

module bob [[interface]];

export
export // { dg-error "occur once" }
void Baz ();

export 
{
  export // { dg-error "occur once" }
    void Bar ();
}

