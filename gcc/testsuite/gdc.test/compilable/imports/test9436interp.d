module imports.test9436interp;

import imports.test9436type;
import imports.test9436aggr;

class ReferenceValueT(T)
{
    void doCast()
    {
        auto x = Type.ConversionFlags.kAllowBaseClass;
    }
}

class ClassValue : ReferenceValueT!Class
{
}
