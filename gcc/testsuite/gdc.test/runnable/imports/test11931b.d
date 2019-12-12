module imports.test11931b;

import imports.test11931c;
import imports.test11931d;

class GUIElement
{
public:
    Signal!void onSubmit;
}

mixin template GUIManager()
{
public:
    void foo()
    {
        onLeftUp.add(&guiUp);
    }

    void guiUp() {}
}
