module imports.test11931a;

import std.stdio;

import imports.test11931d;
import imports.test11931b;

final class Engine
{
package:
    mixin GUIManager;
public:
    Signal!void onLeftUp;
}
