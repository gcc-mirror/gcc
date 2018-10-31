module imports.ice4481b;

import imports.ice4481a;

class Font
{
public:
    int charHeight(dchar c) { return c == 's'; }
    int textHeight(in string text)
    {
        auto maxHeight = (dchar ch) { return charHeight(ch); };
        return reduce!(maxHeight)(text);
    }
}
