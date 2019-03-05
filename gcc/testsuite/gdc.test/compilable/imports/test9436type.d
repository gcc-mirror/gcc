module imports.test9436type;

import imports.test9436node;

class Type
{
    mixin ForwardCtor!();

    enum ConversionFlags
    {
        kAllowBaseClass = 0
    }
}
