// EXTRA_SOURCES: imports/ice4481a.d imports/ice4481b.d

import imports.ice4481a;
import imports.ice4481b;

void main()
{
    auto f = new Font();
    assert(f.textHeight("str"));
}
