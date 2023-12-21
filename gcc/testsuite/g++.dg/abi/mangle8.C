// Red Hat bugzilla 65035
// Bug: We were encoding the name of the instantiation as 'operator int'
// rather than 'operator T'.
// { dg-do compile }
// { dg-additional-options -fabi-compat-version=0 }

struct C {
    template <class T>
    operator T ();
};

template <class T>
C::operator T () { return 0; }

template C::operator int ();

// { dg-final { scan-assembler _ZN1CcvT_IiEEv } }
